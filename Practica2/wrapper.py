from py3270 import Emulator
import os.path
import time


class Wrapper:
    __fichero="/home/luisgg/Documents/Sistemas_Legados/practicas/Sistemas_Legados/Practica2/file.html"
    em=Emulator(visible=True)
    __mainframe='155.210.152.51:103'

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

    def isMore(self):
        m = self.em.string_get(43,71,4)
        if m == "More":
            print("More")
            self.em.send_enter()
            time.sleep(3)

    def exitMainFrame(self):
        self.isMore()
        self.em.send_string('3', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.send_enter()
        self.em.terminate()

    def mainMenu(self):
        self.isMore()
        self.em.send_string('3', ypos=None, xpos=None)
        self.em.send_enter()
        time.sleep(3)

    def assignTask(self):
        self.isMore()
        self.em.send_string('1', ypos=None, xpos=None)
        self.em.send_enter()

    def viewTask(self):
        self.isMore()
        self.em.send_string('2', ypos=None, xpos=None)
        self.em.send_enter()



    def readTask(self,set):
        i=1
        while i < 41 :
            #Compruebo si la linea es una tarea
            isTask=self.em.string_found(i,1,'TASK')
            if isTask:
                newTask=self.em.string_get(i,1,80)
                set.add(newTask)
            i=i+1
        print('OK ' + str(i))
        print(set)
        return set

    def viewSpecificTask(self):
        self.viewTask()
        self.isMore()
        self.em.send_string('2', ypos=None, xpos=None)
        self.em.send_enter()
        #Falta por completar
        time.sleep(3)
        #set de tareas -> si ya esta la tarea -> no la vuelve a añadir
        tareas=set()
        #simulamos do while
        while True:
            tareas=self.readTask(tareas)
            more = self.em.string_get(43,71,4)
            if more != "More":
                print("out")
                break
            else:
                print("in")
                self.em.send_enter()
                time.sleep(1)
        print("estoy fuera")
        print(tareas)
        self.mainMenu()

    def viewGeneralTask(self):
        self.viewTask()
        self.isMore()
        self.em.send_string('1', ypos=None, xpos=None)
        self.em.send_enter()
        time.sleep(3)
        #set de tareas -> si ya esta la tarea -> no la vuelve a añadir
        tareas=set()
        #simulamos do while
        while True:
            tareas=self.readTask(tareas)
            more = self.em.string_get(43,71,4)
            if more != "More":
                print("out")
                break
            else:
                print("in")
                self.em.send_enter()
                time.sleep(1)
        print("estoy fuera")
        print(tareas)
        self.mainMenu()
        #Falta por completar
    

    def generalTask(self,date,description):
        self.assignTask()
        self.isMore()
        self.em.send_string('1', ypos=None, xpos=None)
        self.em.send_enter()
        time.sleep(1)
        self.isMore()
        self.em.send_string(date, ypos=None, xpos=None)
        self.em.send_enter()
        time.sleep(1)
        self.isMore()
        self.em.send_string(description, ypos=None, xpos=None)
        self.em.send_enter()
        time.sleep(1)
        self.mainMenu()

    #Se ha vuelto loco
    def specificTask(self,date,name,description):
        self.assignTask()
        time.sleep(2)
        self.isMore()
        self.em.send_string('2', ypos=None, xpos=None)
        self.em.send_enter()
        time.sleep(2)
        self.isMore()
        self.em.send_string(date, ypos=None, xpos=None)
        self.em.send_enter()
        time.sleep(2)
        self.isMore()
        self.em.send_string(name, ypos=None, xpos=None)
        self.em.send_enter()
        time.sleep(2)  
        self.isMore()      
        self.em.send_string(description, ypos=None, xpos=None)
        self.em.send_enter()
        time.sleep(2)
        self.mainMenu()

  








#IMPORTANTE: HACER ESTO EN UNA FUNCION, ES UNA API

#def iniciar():
    #todo lo de abajo

#em = Emulator()
wrapero = Wrapper()
wrapero.iniciar()
#wrapero.assignTask()
time.sleep(3)

wrapero.specificTask("2134","A","Aprr")
wrapero.specificTask("2134","B","Aprob")
wrapero.specificTask("2134","C","Aprob")
wrapero.specificTask("2134","D","A")
time.sleep(5)
wrapero.viewSpecificTask()
#Tenemos que esperar a que cargue todo lo de ejecutar tareas.c -> (sleep?)

time.sleep(10)
wrapero.mainMenu()
time.sleep(10)
wrapero.exitMainFrame()

