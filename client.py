import pygame,os,sys,time
from pygame.locals import *
from pygame import gfxdraw
import socket
import pygame_textinput
import sys


TCP_IP = '127.0.0.1'
TCP_PORT = 8000
BUFFER_SIZE = 1024
WINDOW_SIDE = 500


def send_message(sock,msg):
    try:
        sock.sendall(msg.encode('utf-8'))
    except socket.error:
        print ('No se pudo enviar el mensaje',msg)


def play(x,y,sock,lastGame):
    top = (15*WINDOW_SIDE)//100
    width_square = (60*WINDOW_SIDE)//300
    width_line = (5*WINDOW_SIDE)//300
    init_pos = top
    for i in list(range(1,4)):
        for j in list(range(1,4)):
            if lastGame!="" and x>=width_square*(i-1)+top+width_line*(i-1)\
            and x<= width_square*i+top+width_line*(i-1) \
            and y>= width_square*(j-1)+top+width_line*(j-1) \
            and y<= width_square*j+top++width_line*(j-1):
                cmd = "PLA "+lastGame+" "+str(i)+" "+str(j)
                send_message(sock,cmd)
                break


def draw(screen,str):
    color = (122,0,255)
    top = (15*WINDOW_SIDE)//100
    width_square = (60*WINDOW_SIDE)//300
    width_line = (5*WINDOW_SIDE)//300
    init_pos = top
    i = 0
    while i<2 :
        init_pos+=width_square
        pygame.gfxdraw.box(screen,Rect(top,init_pos,(width_square*3+width_line*2),width_line),color)
        pygame.gfxdraw.box(screen,Rect(init_pos,top,width_line,(width_square*3+width_line*2)),color)
        init_pos+=width_line
        i+=1

    ccolor = (222,211,255)
    rcolor = (222,211,5)

    i = 0
    radius = ((width_square//2)*7)//10
    for c in str:
        x = (i%3)*width_square + top + (width_square//2) + (width_line*(i%3))
        y = (i//3)*width_square + top + (width_square//2) + (width_line*(i//3))
        if c == "O":
            pygame.gfxdraw.filled_circle(screen,x,y,radius,ccolor)
        if c == "X":
            pygame.gfxdraw.box(screen,Rect(x-radius,y-radius,2*radius,2*radius),rcolor)
        if  c!="\n":
            i+=1


def main():

    global TCP_PORT
    TCP_PORT = int(sys.argv[1])
    global WINDOW_SIDE
    if len(sys.argv) == 3:
        WINDOW_SIDE = int(sys.argv[2])
    print("Size:",WINDOW_SIDE)
    pygame.init()
    screen = pygame.display.set_mode((WINDOW_SIDE,WINDOW_SIDE))
    pygame.display.set_caption("TaTeTi")
    textinput = pygame_textinput.TextInput(font_size = min((7*WINDOW_SIDE)//100,35))

    pygame.font.init()
    myfont = pygame.font.SysFont('Comic Sans MS', (5*WINDOW_SIDE)//100)

    # ------ Socket --------
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_address = (TCP_IP, TCP_PORT)
    sock.connect(server_address)
    sock.setblocking(0)
    # ----------------------

    cmd = ""
    lastCommand = ""
    serverMsg = ""
    board = ""
    lastGameInteraction = ""
    userName = ""

    while True:

        screen.fill((255,255,255))
        draw(screen,board)

        events = pygame.event.get()
        for event in events:
            if event.type == MOUSEBUTTONDOWN :
                x, y = event.pos
                play(y,x,sock,lastGameInteraction)
            if event.type == pygame.QUIT:
                exit()

        textinput.update(events)
        cmd = textinput.get_text()

        screen.blit(textinput.get_surface(), (10, 10))

        textsurface = myfont.render(serverMsg.translate({ord('\n'): " "}), False, (0, 0, 0))

        top = (15*WINDOW_SIDE)//100
        response_pos = (15*WINDOW_SIDE)//100
        screen.blit(textsurface,(top,WINDOW_SIDE-(top/2)))


        if len(cmd) and cmd[-1] == '.' :
            cmd = cmd[:-1]
            lastCommand = cmd
            aux = cmd.split()
            if((aux[0] == "PLA" or aux[0] == "ACC")and len(aux)>=2):
                lastGameInteraction = aux[1]
            if(aux[0] == "NEW"):
                lastGameInteraction = userName
            send_message(sock,cmd)
            textinput = pygame_textinput.TextInput(font_size = min((7*WINDOW_SIDE)//100,35))

        pygame.display.flip()

        try:
            data = sock.recv(BUFFER_SIZE)
            str1 = data.decode("UTF-8")
            aux = str1.split("\n")
            if lastCommand!= "" and "CON" == lastCommand.split()[0] and userName == "" and str1 == "Bienvenido!\n":
                userName = lastCommand.split()[1]
            if "TABLERO" == aux[0]:
                board = str1[7:19]
                if(str1[19:]!=""):
                    serverMsg = str1[19:]
            else:
                serverMsg = str1
        except socket.error:
            i = 0



if __name__ == "__main__":
    main()
