from py3270 import Emulator
import os.path
import time


def __waitScreen():
    time.sleep(3)


class Wrapper:
    __fichero="/home/luisgg/Documents/Sistemas_Legados/practicas/Sistemas_Legados/Practica2/file.html"
    em=Emulator(visible=True)
    __mainframe='155.210.152.51:104'

    def iniciar(self):       
        self.em.connect(self.__mainframe)
        self.em.send_enter()
        time.sleep(2)
        self.em.fill_field(3,18,'grupo_04',8)
        self.em.fill_field(5,18,'secreto6',8)
        self.em.send_enter()
        time.sleep(3)
        self.em.send_enter()
        time.sleep(3)
        self.em.fill_field(3,15,'tareas.c',8)
        self.em.send_enter()

    def exitMainFrame(self):
        self.em.send_string('3', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.send_enter()
        self.em.terminate()

    def mainMenu(self):
        self.em.send_string('3', ypos=None, xpos=None)
        self.em.send_enter()

    def assignTask(self):
        self.em.send_string('1', ypos=None, xpos=None)
        self.em.send_enter()

    def viewTask(self):
        self.em.send_string('2', ypos=None, xpos=None)
        self.em.send_enter()

    def viewSpecificTask(self):
        self.viewTask()
        self.em.send_string('2', ypos=None, xpos=None)
        self.em.send_enter()
        #Falta por completar

    def viewGeneralTask(self):
        self.viewTask()
        self.em.send_string('1', ypos=None, xpos=None)
        self.em.send_enter()
        #Falta por completar
    

    def generalTask(self,date,description):
        self.assignTask()
        self.em.send_string('1', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.send_string(date, ypos=None, xpos=None)
        self.em.send_enter()
        self.em.send_string(description, ypos=None, xpos=None)
        self.em.send_enter()
        self.mainMenu()

    #Se ha vuelto loco
    def specificTask(self,date,name,description):
        self.assignTask()
        self.em.send_string('2', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.send_string(date, ypos=None, xpos=None)
        self.em.send_enter()
        self.em.send_string(name, ypos=None, xpos=None)
        self.em.send_enter()        
        self.em.send_string(description, ypos=None, xpos=None)
        self.em.send_enter()
        self.mainMenu()

  








#IMPORTANTE: HACER ESTO EN UNA FUNCION, ES UNA API

#def iniciar():
    #todo lo de abajo

#em = Emulator()
wrapero = Wrapper()
wrapero.iniciar()
wrapero.assignTask()
time.sleep(3)
wrapero.specificTask("2134","Bd","Aprobar")


wrapero.viewSpecificTask()
#Tenemos que esperar a que cargue todo lo de ejecutar tareas.c -> (sleep?)

time.sleep(10)
wrapero.mainMenu()
time.sleep(10)
wrapero.exitMainFrame()

