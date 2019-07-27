-module(server).
-compile(export_all).
%-export([init/0,init/1,init/2]).
-define(PORT, 8000).
-define(InitialBoard,[0,"         "]).

% Espera 1 segundo.
wait(Ms) -> receive after 1000 * Ms -> ok end.

% Retorna una lista con todos los nodos del sistema.
knownNodes() -> [node()|nodes()].

% Cada un segundo manda su carga a los demas.
pStat() ->
  {_,Load} = statistics(reductions),
  lists:foreach(fun(Node) -> {balance, Node} ! {info, node(), Load} end, knownNodes()),
  wait(1),
  pStat().

% Hace un update en un diccionario ordenado de cargas.
updateLoads(Dict, Key, Value) ->
  orddict:update(Key, fun(_)->Value end, Value, Dict).

% Calcula cual es el nodo con menor carga del diccionario,
% mediante un fold y lo retorna.
bestNode(NodesLoads) ->
  {BestNode,_} = orddict:fold(fun(A,ALoad,{B,BLoad}) ->
               if ALoad < BLoad -> {A,ALoad};
               true ->  {B,BLoad} end end,
               {base,1.0e11},NodesLoads),
  BestNode.

% Retorna el nodo con menor carga que devuelve
% el proceso balance.
bestNode() ->
              balance ! {getBest,self()},
              receive BestNode -> BestNode end.

% Tiene 2 funciones:
% - Actualizar la información de los nodos.
% - Retornar el nodo con menos carga.
pBalance(NodesLoads) ->
	receive
		{getBest, Pid} ->
			Pid ! bestNode(NodesLoads), pBalance(NodesLoads);
		{info, Node, Load} ->
			pBalance(updateLoads(NodesLoads, Node, Load))
	end.

% Dispatcher: Cuando un cliente quiere comunicarse, crea un el proceso
% pScket para que se comunique con el.

% Usa el puerto por defecto.
dispatcher() ->
  case gen_tcp:listen(?PORT, [{active, false}]) of
            {ok, ListenSocket} -> dispatcher(ListenSocket,0);
            {error, Reason} ->
              io:format("No se pudo crear la conexión, ~p.~n", [Reason]),
              error
  end.

% Usa el Puerto especificado.
dispatcher(Port) ->
  case gen_tcp:listen(Port, [{active, false}]) of
            {ok, ListenSocket} -> dispatcher(ListenSocket,0);
            {error, Reason} ->
              io:format("No se pudo crear la conexión, ~p.~n", [Reason]),
              error
  end.

dispatcher(ListenSocket,N) ->
  {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
  io:format("Se conecto el cliente ~p ~n",[N]),
  spawn(?MODULE, pSocket, [ClientSocket,N]),
  dispatcher(ListenSocket,N+1).

% Retorna una lista de TODOS los usuarios (no solo del nodo).
globalUsers(Users) ->
    Users ++ lists:append(lists:map( fun(Node)->
            {usersManager, Node} ! {localUsers, self()},
            receive Usrs -> Usrs end end,
        nodes() )).

% Es el proceso encargado de manejar la lista de Usuarios.
% Este puede crear o eliminar usuarios.
% Tienen una lista de Usuarios Locales (los usuarios que fueron
% creados dentro del nodo) como parametro.
% Los elementos de esta lista son los nombres de los Usuarios.
pUsersManager(Users) ->
  receive
      {new,Name,Pid}     ->   case lists:member(Name,globalUsers(Users)) of
                                true ->
                                  Pid ! error, pUsersManager(Users);
                                false ->
                                  Pid ! ok, pUsersManager([Name|Users])
                              end;

      {delete,Name}     -> pUsersManager(Users--[Name]);

      {localUsers, Pid} -> Pid ! Users, pUsersManager(Users)
  end.

% PSocket: Es el proceso que recibe las peticiones de un usuario
% especifico.

% Antes de tener un nombre asignado (es decir antes de "iniciar sesion").
pSocket(Socket,N) ->
  case gen_tcp:recv(Socket, 0) of
      {ok, Msg} ->
           case string:tokens(Msg, " \r\n") of
             ["CON",Name] -> usersManager ! {new,Name,self()},
                             receive
                               ok    -> io:format("Usuario ~p  Registrado ~n",[Name]),
                                        RepPid = spawn(?MODULE,pReplier,[Socket]),
                                        RepPid ! "Bienvenido!\n",
                                        pSocket(Socket,N,Name,RepPid);

                               error -> gen_tcp:send(Socket, "El nombre de usuario ya esta en uso\n"),
                                        pSocket(Socket,N);

                               _     -> gen_tcp:send(Socket, "Comando invalido\n")
                             end;

             _            -> gen_tcp:send(Socket, "No tiene nombre de usuario\n"),
                             pSocket(Socket,N)
           end;
      {error, Reason} ->
        io:format("El cliente ~p no se pudo conectar debido a : ~p ~n",[N,Reason]),
        error
  end.

% Una vez que el usuario se identificó con un nombre.
pSocket(Socket,N,Name,RepPid) ->
  case gen_tcp:recv(Socket, 0) of

    {ok, Cmd}        -> BestNode = bestNode(),
                        io:format("PComando se crea en ~p ~n",[BestNode]),
                        case string:tokens(Cmd, " \t\r\n") of
                              ["BYE"] -> spawn(BestNode,server,pComando,[Name,Cmd,RepPid,node()]),
                                         gen_tcp:close(Socket);
                              _       -> spawn(BestNode,server,pComando,[Name,Cmd,RepPid,node()]),
                                         pSocket(Socket,N,Name,RepPid)
                        end;

    {error, Reason}   -> io:format("El cliente ~p no se pudo
                         conectar debido a : ~p",[N,Reason]),
                         error
  end.

% Proceso encargado de enviarle mensajes al Usuario mediante
% el protocolo TCP IP.
pReplier(Socket) ->
  receive
    die -> gen_tcp:send(Socket,"Chau!\n"),ok;
    Msg -> gen_tcp:send(Socket,Msg),
           pReplier(Socket)
  end.

% Retorna una lista con las partidas creadas en todos los
% nodos (disponibles al momento de pedirla).
globalGames(Games) ->
    Games ++ lists:append(lists:map( fun(Node)->
            {gamesManager, Node} ! {localGames, self()},
            receive Gms -> Gms end end,
        nodes() )).

% Elimina un juego de TODO el servidor.
deleteGame(GameId) ->
      lists:foreach( fun(Node) -> {gamesManager, Node} ! {deleteLocal,GameId} end,nodes()).

% Maneja todas las partidas en el servidor.
% Su parametro es la lista de Juegos Locales del nodo.
% Esta lista esta conformada por {Nombre_Del_Juego,Pid_Del_Juego}.
pGamesManager(Games) ->

    receive

      {new,Name,Pid,RPid}           ->  case proplists:is_defined(Name,Games) of
                                                true  -> Pid ! exists,
                                                         pGamesManager(Games);

                                                false -> BestNode = bestNode(),
                                                         io:format("El Juego se crea en ~p ~n",[BestNode]),
                                                         GamePid = spawn(BestNode,server,game,[{[],[{Name,RPid}],?InitialBoard}]),
                                                         Pid ! created,
                                                         pGamesManager([{Name,GamePid}|Games])
                                       end;


      {deleteLocal,GameId}          -> pGamesManager(proplists:delete(GameId,Games));

      {delete,GameId}               -> case proplists:is_defined(GameId,Games) of
                                              true -> pGamesManager(proplists:delete(GameId,Games));
                                              false -> deleteGame(GameId),pGamesManager(Games)
                                       end;

      {accept,GameId,Name,RPid,Pid} -> case proplists:get_value(GameId,globalGames(Games)) of
                                         undefined -> Pid ! noGame;
                                         GamePid   -> GamePid ! {joinPlay,Name,RPid,self()},
                                                      receive X -> Pid ! X end
                                       end,
                                       pGamesManager(Games);

      {obs,GameId,Name,RPid,Pid}    -> case proplists:get_value(GameId,globalGames(Games)) of
                                          undefined -> RPid ! "No existe esa partida\n";
                                          GamePid   -> GamePid ! {joinObs,Name,RPid}
                                       end,
                                       pGamesManager(Games);

      {leftObs,GameId,Name,RPid}         -> case proplists:get_value(GameId,globalGames(Games)) of
                                          undefined -> RPid ! "No existe esa partida\n";
                                          GamePid   -> RPid ! "Dejaste de observar\n",
                                                       GamePid ! {leftObs,Name}
                                       end,
                                       pGamesManager(Games);

      {play,GameId,Name,Move,RPid}      ->  case proplists:get_value(GameId,globalGames(Games)) of
                                          undefined -> RPid ! "No existe esa partida\n";
                                          GamePid   -> GamePid ! {play,Move,Name}
                                       end,
                                       pGamesManager(Games);

      {globalGames,Pid}             -> Pid ! globalGames(Games),
                                       pGamesManager(Games);

      {localGames,Pid}              -> Pid ! Games,
                                       pGamesManager(Games);

      {bye,Name}                    -> lists:foreach(fun({_,Pid}) -> Pid ! {play,["leftPlay"],Name} end,globalGames(Games)),
                                       pGamesManager(Games)

    end.

% Maneja todos los comandos del usuario.
% Como parametros recibe el nombre del usuario, el comando,
% el PID del proceso encargado de responder al usuario y el
% nodo en el que se encuentra.
pComando(Name,Cmd,RPid,Node) ->
  case string:tokens(Cmd, " \t\r\n") of

    ["LSG"]                 -> {gamesManager,Node} ! {globalGames,self()},
                               receive GlogalGames ->
                                 GameNames = proplists:get_keys(GlogalGames),
                                 GameNames_ = lists:map(fun(Str) -> "-"++Str++"-" end,GameNames),
                                 Table = string:join(["Partidas:"|GameNames_],"\n") ++ "\n",
                                 RPid ! Table
                               end;

    ["NEW"]                 -> {gamesManager,Node} ! {new,Name,self(),RPid},
                                receive
                                     created -> RPid ! "Partida Creada\n";
                                     exists  -> RPid ! "Ya tiene una partida creada\n"
                                end;

    ["ACC", GameId]         -> if GameId == Name ->  RPid ! "Es tu propia partida!\n";
                                            true ->
                               {gamesManager,Node} ! {accept,GameId,Name,RPid,self()},
                               receive
                                      ok     -> RPid ! "Partida Aceptada\n";
                                      noGame -> RPid ! "No existe esa partida\n";
                                      error  -> RPid ! "La partida ya esta llena\n"
                               end end;

    ["PLA", GameId | Move] -> {gamesManager,Node} ! {play,GameId,Name,Move,RPid},
                              io:format("Juego ~p Movimiento ~p ~n",[GameId,Move]);

    ["OBS", GameId]         -> {gamesManager,Node} ! {obs,GameId,Name,RPid,self()};

    ["LEA", GameId]         -> {gamesManager,Node} ! {leftObs,GameId,Name,RPid};

    ["BYE"]                 -> {gamesManager,Node} ! {bye,Name},
                               {usersManager,Node} ! {delete,Name},
                               RPid ! die;

    _                       -> RPid ! "Comando invalido\n"

  end.


% Envia un mensaje a todos los jugadores que estan participando
% del juego.
sendToPlayers(Players,Msg) ->
  lists:foreach(fun({_,Pid})->Pid!Msg end,Players).

% Envia un mensaje a un jugar en especifico.
sendToPlayer(Players,Name,Msg) ->
  case proplists:lookup(Name,Players) of
   {_,Pid} -> Pid ! Msg
 end.

% Dado el nombre del jugador, verifica que sea su turno
% de jugar.
turnValidation(Players,Name,N) ->
  case Players of
    [{Name,_}]           -> onePlayer;

    [{Name,_},_] -> if (N rem 2) == 0 -> ok;
                                 true -> noTurn
                              end;

    [_,{Name,_}] -> if (N rem 2) == 1 -> ok;
                                 true -> noTurn
                              end;

    _                   -> io:format("No esta en el juego ~n"),
                           error
  end.

% Dado el nombre del jugador retorna el simbolo que le
% corresponde en el tablero.
symbol(Players,Name) ->
  case Players of
    [{Name,_},_] -> "X";
    _            -> "O"
  end.

% Dado el tablero y el simbolo correspondiente
% a uno de los jugadores retorna si gano o no.
win(Board,S) ->
  case Board of
    [S,S,S,_,_,_,_,_,_] -> true;
    [_,_,_,S,S,S,_,_,_] -> true;
    [_,_,_,_,_,_,S,S,S] -> true;
    [S,_,_,_,S,_,_,_,S] -> true;
    [_,_,S,_,S,_,S,_,_] -> true;
    [_,_,S,_,_,S,_,_,S] -> true;
    [S,_,_,S,_,_,S,_,_] -> true;
    [_,S,_,_,S,_,_,S,_] -> true;
    _                   -> false
  end.

% Se fija si se termino el juego, es decir si hubo alguien que
% gano, si se empato o si se continua el juego,
gameOver(Board,N,S) ->
  case {N,win(Board,S)} of
         {_,true} -> winner;
         {9,_}    -> tie;
         _        -> continue
  end.

% Toma la string del tablero y lo retorn con espacios,
% para que sea mas visible.
boardPresentation(Board) ->
      lists:sublist(Board,1,3) ++ "\n"
      ++ lists:sublist(Board,4,3) ++ "\n"
      ++ lists:sublist(Board,7,3) ++ "\n".

% Es el proceso que maneja una partida de TaTeTi, como parametro
% tiene una 3-upla, su primer elemento es una lista de observadores
% sus elementos tienen la forma {Nombre_Obs,Pid_Replier_Obs}, el segundo
% elemento tiene la lista de jugadores sus elementos tiene la forma
% {Nombre_Jugador,Pid_Replier_Jugador}, y su ultimo elemento es el tablero
% una lista con 2 elementos el numero de turno en el que se encuentra la
% partida y una string que representa el tablero.
game({Observers,Players,Board}) ->
  receive

    {close,GameId}             -> gamesManager ! {delete,GameId};

    {reset,Player}            -> game({Observers,[Player|[]],?InitialBoard});

    {reset}                   -> game({Observers,Players,?InitialBoard});

    {joinPlay,Name,RPid,Pid} -> if length(Players) < 2 ->
                                      Pid ! ok,
                                      game({Observers,[{Name,RPid}|Players],Board});
                                    true -> Pid ! error,
                                    game({Observers,Players,Board})
                                end;

    {joinObs,Name,RPid}      -> case proplists:is_defined(Name,Observers) of
                                     true  -> RPid ! "Ya la estas observando!\n",
                                              game({Observers,Players,Board});
                                     false -> case Board of
                                                   [_|[Brd]] -> BP = boardPresentation(Brd),
                                                   sendToPlayer([{Name,RPid}|Observers],Name,"TABLERO\n"++BP)
                                              end,
                                              RPid ! "Te uniste a observar\n",
                                              game({[{Name,RPid}|Observers],Players,Board})
                                end;

    {leftObs,Name}           -> game({proplists:delete(Name,Observers),Players,Board});

    {play,["leftPlay"],Name}   -> case Players of

                                     [{Name,_}|[B|[]]] -> self() ! {reset,B},
                                                          game({Observers,Players,Board});

                                     [_A|[{Name,_}|[]]] -> self() ! {close,Name},
                                                          game({Observers,Players,Board});

                                     [{Name,_}|[]]     -> self() ! {close,Name},
                                                          game({Observers,Players,Board});

                                     _                 -> game({Observers,Players,Board})
                                  end;

    {play,[RC,CC],Name}   -> case Board of
                                [N,B] -> case turnValidation(Players,Name,N) of

                                              ok -> R = hd(RC) - 48,
                                                    C = hd(CC) - 48,
                                                    if ((R > -1) and (R < 4) and (C > -1) and (C < 4)) ->
                                                      S = lists:nth(3*(R-1) + C,B),
                                                      if S == 32 ->
                                                         Brd = lists:sublist(B,3*(R-1)+C-1)
                                                               ++ symbol(Players,Name)
                                                               ++ lists:nthtail(3*(R-1)+C,B),
                                                         BP = boardPresentation(Brd),
                                                         sendToPlayers(Players,"TABLERO\n"++BP),
                                                         sendToPlayers(Observers,"TABLERO\n"++BP),

                                                         case gameOver(Brd,N+1,hd(symbol(Players,Name))) of
                                                           winner -> sendToPlayer(Players,Name,"Ganaste! El juego se reiniciara.\n"),
                                                                     sendToPlayers(proplists:delete(Name,Players),"Perdiste :(, El juego se reiniciara.\n"),
                                                                     self() ! {reset},game({Observers,Players,[N+1,Brd]});
                                                           tie -> sendToPlayers(Players,"Empate! El juego se reiniciara.\n"),
                                                                  self() ! {reset},game({Observers,Players,[N+1,Brd]});
                                                           continue -> game({Observers,Players,[N+1,Brd]})
                                                         end;
                                                         true -> sendToPlayer(Players,Name,"Jugada invalida.\n"),
                                                                 game({Observers,Players,Board})
                                                      end;
                                                    true -> sendToPlayer(Players,Name,"Jugada invalida.\n"),
                                                            game({Observers,Players,Board})
                                                    end;
                                              noTurn -> sendToPlayer(Players,Name,"No es tu turno.\n"),
                                                       game({Observers,Players,Board});
                                              error -> game({Observers,Players,Board});
                                              onePlayer -> sendToPlayer(Players,Name,"Esperando a otro jugador.\n"),
                                                           game({Observers,Players,Board})
                                          end
                          end;
    _ -> game({Observers,Players,Board})

  end.

% Inicia un nodo con el puerto por defecto.
init() ->
  spawn(?MODULE,pStat,[]),
  spawn(?MODULE,dispatcher,[]),
  register(balance, spawn(?MODULE, pBalance, [orddict:new()])),
  register(usersManager,spawn(?MODULE,pUsersManager,[[]])),
  register(gamesManager,spawn(?MODULE,pGamesManager,[[]])).

% Inicia un nodo con el puerto especificado.
init(Port) ->
  spawn(?MODULE,pStat,[]),
  spawn(?MODULE,dispatcher,[Port]),
  register(balance, spawn(?MODULE, pBalance, [orddict:new()])),
  register(usersManager,spawn(?MODULE,pUsersManager,[[]])),
  register(gamesManager,spawn(?MODULE,pGamesManager,[[]])).

% Intenta conectarse con los demas nodos hasta que lo logra.
connect(Node) ->
  case net_adm:ping(Node) of
    pang -> io:format("Intentando Conectar...~n"),connect(Node);
    pong -> io:format("Conectado.~n")
  end.

% Inicia un nodo con el puerto especificado, conectandolo con
% el nodo pasado por argumento.
init(Port,Node) ->
  connect(Node),
  spawn(?MODULE,pStat,[]),
  spawn(?MODULE,dispatcher,[Port]),
  register(balance, spawn(?MODULE, pBalance, [orddict:new()])),
  register(usersManager,spawn(?MODULE,pUsersManager,[[]])),
  register(gamesManager,spawn(?MODULE,pGamesManager,[[]])).
