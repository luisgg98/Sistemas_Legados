from py3270 import Emulator
import os.path
import time


class Wrapper:
    #Atributos de la clase
    __fichero="/home/luisgg/Documents/Sistemas_Legados/practicas/Sistemas_Legados/Practica2/file.html"
    em=Emulator(visible=True)
    __mainframe='155.210.152.51:103'
    timeout=1

    #Funcion para iniciar la conexión, iniciar sesion y ejecutar
    #   tareas.c
    def iniciar(self):       
        self.em.connect(self.__mainframe)
        self.em.wait_for_field()
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.em.fill_field(3,18,'grupo_04',8)
        self.em.fill_field(5,18,'secreto6',8)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)
        
        self.em.fill_field(3,15,'tareas.c',8)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)



    #Funcion interna para comprobar si abajo a la derecha aparece "More"
    #   si True, aparece y pulsa enter
    def isMore(self):
        m = self.em.string_get(43,71,4)
        if m == "More":
            print("More")
            self.em.send_enter()
            self.em.wait_for_field()
            time.sleep(self.timeout)
            #time.sleep(3)




    #Termina la ejecucion de tareas.c y vuelve al menu principal del
    #   mainframe. 
    #   Si salimos -> perdemos las tareas almacenadas (no persistente)
    def exitMainFrame(self):
        self.isMore()
        self.em.send_string('3', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)
        self.em.terminate()



    #Volver al menu principal
    def mainMenu(self):
        self.isMore()
        self.em.send_string('3', ypos=None, xpos=None)
        self.em.send_enter()
        time.sleep(3)



    #Desde el menu principal, avanza a la operacion "Crear tarea"
    def assignTask(self):
        self.isMore()
        self.em.send_string('1', ypos=None, xpos=None)
        self.em.send_enter()



    #INTRODUCIR NUEVA TAREA GENERAL
    #   IN:     fecha, nombre
    #   OUT:    nada
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



    #INTRODUCIR NUEVA TAREA ESPECIFICA
    #   IN:     fecha, nombre, descripcion
    #   OUT:    nada
    def specificTask(self,date,name,description):
        self.assignTask()
        #time.sleep(2)
        self.isMore()
        self.em.send_string('2', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)
        

        self.isMore()
        self.em.send_string(date, ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.isMore()
        self.em.send_string(name, ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.isMore()      
        self.em.send_string(description, ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.mainMenu()



    #Desde el menu principal, avanza a la operacion "Ver tarea"
    def viewTask(self):
        self.isMore()
        self.em.send_string('2', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)



    #Funcion interna para leer las tareas presentes en una pantalla
    #   Nota: en caso de estar mostrando dos veces la lista en la 
    #   pantalla, se queda con la mas completa
    def readTask(self,list,tipo):
        i=1
        while i < 41 :
            #Compruebo si la linea es una tarea
            isTask=self.em.string_found(i,1,'TASK')
            if isTask:
                isTipo=self.em.string_found(i,9,tipo)
                if isTipo:
                    newTask=self.em.string_get(i,1,80)
                    print(newTask)
                    if newTask in list:
                        pass
                    else:
                        list.append(newTask)
                    
            i=i+1
        print('OK ' + str(i))
        print(list)
        time.sleep(self.timeout)
        return list



    #LISTAR TAREAS GENERALES
    #   IN:     sin argumentos
    #   OUT:    lista de listas. Cada componente una tarea.
    #           [[fecha1,nombre1,des1],[fecha2,nombre2,des2]]
    #           Nota: nombreX sera la cadena vacia porque las tareas 
    #           generales carecen de descripcion
    def viewGeneralTask(self):
        self.viewTask()
        self.isMore()
        self.em.send_string('1', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

       
        tareas=list()
        #simulamos do while
        while True:
            tareas=self.readTask(tareas,"GENERAL")
            more = self.em.string_get(43,71,4)
            if more != "More":
                print("out")
                break
            else:
                print("in")
                self.em.send_enter()
                self.em.wait_for_field()
                time.sleep(self.timeout)
        print("estoy fuera")
        print(tareas)
        self.mainMenu()



    #LISTAR TAREAS ESPECIFICAS
    #   IN:     sin argumentos
    #   OUT:    lista de listas. Cada componente una tarea.
    #           [[fecha1,nombre1,des1],[fecha2,nombre2,des2]]
    def viewSpecificTask(self):
        self.viewTask()
        self.isMore()
        self.em.send_string('2', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        tareas=list()
        #simulamos do while
        while True:
            tareas=self.readTask(tareas,"SPECIFIC")
            more = self.em.string_get(43,71,4)
            if more != "More":
                print("prueba")
                break
            else:
                print("in")
                self.em.send_enter()
                self.em.wait_for_field()
                time.sleep(self.timeout)
        print("estoy fuera")
        print(tareas)
        self.mainMenu()


    
    

  








#IMPORTANTE: HACER ESTO EN UNA FUNCION, ES UNA API

#def iniciar():
    #todo lo de abajo

#em = Emulator()
wrapero = Wrapper()
wrapero.iniciar()
#wrapero.assignTask()
time.sleep(3)

wrapero.generalTask("1234", "prueba")

wrapero.viewGeneralTask()
wrapero.viewGeneralTask()
wrapero.viewSpecificTask()

#wrapero.specificTask("2134","A","Aprr")
#wrapero.specificTask("2134","B","Aprob")
#wrapero.specificTask("2134","C","Aprob")
#wrapero.specificTask("2134","D","ok")
#wrapero.specificTask("2134","E","ok")
#wrapero.specificTask("2134","F","ok")
#time.sleep(3)
#wrapero.viewSpecificTask()
#Tenemos que esperar a que cargue todo lo de ejecutar tareas.c -> (sleep?)

time.sleep(10)
wrapero.mainMenu()
time.sleep(10)
wrapero.exitMainFrame()

