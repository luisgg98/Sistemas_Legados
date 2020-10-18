from py3270 import Emulator
import os.path
import time

class Wrapper:
    #Atributos de la clase
    em=Emulator(visible=True)
    __mainframe='155.210.152.51:103'
    timeout=0.25
    __grupo='grupo_04'
    __password='secreto6'

    def fechasTruncate(self,date):
        lista_date=date.split('/')
        date= lista_date[0] + lista_date[1]
        return date

    def descriptionTruncate(self,description): 
        description = description.replace(" ", "_") 
        if len(description) > 12:
            description=description[0:11]
 
        return description

    def nameTruncate(self,name):
        name= name.replace(" ", "_")
        if len(name) > 5:
            name=name[0:4]  
        return name

    #Funcion para iniciar la conexion, iniciar sesion y ejecutar
    #   tareas.c
    def iniciar(self):       
        self.em.connect(self.__mainframe)
        self.em.wait_for_field()
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.em.fill_field(3,18,self.__grupo,8)
        self.em.fill_field(5,18,self.__password,8)
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
            #print("More")
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
        self.em.wait_for_field()
        time.sleep(self.timeout)        


    #Desde el menu principal, avanza a la operacion "Crear tarea"
    def assignTask(self):
        self.isMore()
        self.em.send_string('1', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

    #INTRODUCIR NUEVA TAREA GENERAL
    #   IN:     fecha, nombre
    #   OUT:    nada
    def generalTask(self,date,description):
        date=self.fechasTruncate(date)
        description=self.descriptionTruncate(description)

        self.assignTask()
        self.isMore()
        self.em.send_string('1', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.isMore()
        self.em.send_string(date, ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.isMore()
        self.em.send_string(description, ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)
        
        self.mainMenu()

    #INTRODUCIR NUEVA TAREA ESPECIFICA
    #   IN:     fecha, nombre, descripcion
    #   OUT:    nada
    def specificTask(self,date,name,description):
        date=self.fechasTruncate(date)
        description=self.descriptionTruncate(description)
        name=self.nameTruncate(name)

        self.assignTask()
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
                isTipo2=self.em.string_found(i,10,tipo)
                isTipo3=self.em.string_found(i,11,tipo)
                if isTipo or isTipo2 or isTipo3:
                    newTask=self.em.string_get(i,1,80)
                    if newTask in list:
                        pass
                    else:
                        list.append(newTask)
                    
            i=i+1
        time.sleep(self.timeout)
        return list


    #FUNCION que convierte a lista de listas para devolver a frontend en 
    #   el formato acordado
    def listOfLists(self, lista):
        result=list()
        for tarea in lista:
            contenido=tarea.split(' ')
            contenido[5]=contenido[5].replace("_", " ")
            contenido[4]=contenido[4].replace("_", " ")
            info=contenido[3:6]
            nTarea=contenido[1].split(':')
            nTarea=nTarea[0]
            newTarea=[nTarea]+info
            result.append(newTarea)
        return result

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
        #print(tareas)
        #Convertimos a lista de listas
        result=self.listOfLists(tareas)
        self.mainMenu()

        return result

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
        #print(tareas)
        #Convertimos a lista de listas
        result=self.listOfLists(tareas)

        self.mainMenu()

        return result


#IMPORTANTE: HACER ESTO EN UNA FUNCION, ES UNA API

#def iniciar():
    #todo lo de abajo

#em = Emulator()
wrapero = Wrapper()
wrapero.iniciar()
#wrapero.assignTask()
time.sleep(3)

#wrapero.generalTask("1234", "A")
#wrapero.generalTask("1234", "B")
#wrapero.generalTask("1234", "C")
#wrapero.generalTask("1234", "D")

tarea=0
while tarea < 129:
    wrapero.generalTask("01/02/04","Tarea  "+ str(tarea))
    tarea = tarea + 1 

t=time.time()
r=wrapero.viewGeneralTask()
t=time.time() - t
print(t)
print(r)
print("------------------------------------")

time.sleep(10)
wrapero.mainMenu()
time.sleep(10)
wrapero.exitMainFrame()

