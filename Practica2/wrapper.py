from py3270 import Emulator
import os.path
import time


class Wrapper:
    # Atributos de la clase
    em = Emulator(visible=True)
    __mainframe = '155.210.152.51:103'
    timeout = 0.25
    timeout_iniciar = 2.25
    __grupo = 'grupo_04'
    __password = 'secreto6'

    #########################################################################################
    #   PRIVATE METHODS
    #########################################################################################

    # PRIVATE:   obtener la fecha en formato ddmm
    def __fechasTruncate(self, date):
        lista_date = date.split('/')
        date = lista_date[0] + lista_date[1]
        return date

    # PRIVATE:   descripcion es <=12
    #           si supera la longitud, la trunca
    def __descriptionTruncate(self, description):
        description = description.replace(" ", "_")
        if len(description) > 12:
            description = description[0:11]

        return description

    # PRIVATE:   nombre es <=5
    #           si supera la longitud, la trunca
    def __nameTruncate(self, name):
        name = name.replace(" ", "_")
        if len(name) > 5:
            name = name[0:5]
        return name

    # PRIVATE:   comprobar si abajo a la derecha aparece "More"
    #           si True, aparece y pulsa enter
    def __isMore(self):
        m = self.em.string_get(43, 71, 4)
        if m == "More":
            self.em.send_enter()
            self.em.wait_for_field()
            time.sleep(self.timeout)
            # time.sleep(3)

    # PRIVATE:   volver al menu principal
    def __mainMenu(self):
        self.__isMore()
        self.em.send_string('3', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        # PRIVATE:   avanza a la operacion "Crear tarea"

    def __assignTask(self):
        self.__isMore()
        self.em.send_string('1', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

    # PRIVATE:   avanza a la operacion "Ver tarea"
    def __viewTask(self):
        self.__isMore()
        self.em.send_string('2', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

    # PRIVATE:   leer las tareas presentes en una pantalla
    #           Nota: en caso de estar mostrando dos veces la lista en la 
    #           pantalla, se queda con la mas completa
    def __readTask(self, list, tipo):
        i = 1
        while i < 41:
            # Compruebo si la linea es una tarea
            isTask = self.em.string_found(i, 1, 'TASK')
            if isTask:
                isTipo = self.em.string_found(i, 9, tipo)
                isTipo2 = self.em.string_found(i, 10, tipo)
                isTipo3 = self.em.string_found(i, 11, tipo)
                if isTipo or isTipo2 or isTipo3:
                    newTask = self.em.string_get(i, 1, 80)
                    if newTask in list:
                        pass
                    else:
                        list.append(newTask)

            i = i + 1
        time.sleep(self.timeout)
        return list

    # PRIVATE:   devuelve la fecha pasada como "ddmm" en el formato "dd/mm"
    def __dateFormat(self, d):
        day = int(d) // 100
        if day < 10:
            day = '0' + str(day)
        else:
            day = str(day)

        month = int(d) % 100
        if month < 10:
            month = '0' + str(month)
        else:
            month = str(month)

        date = day + '/' + month
        return date

    # PRIVATE:   convierte las tareas a lista de listas para devolver en
    #           el formato acordado
    def __listOfLists(self, lista):
        result = list()
        for tarea in lista:
            contenido = tarea.split(' ')
            # Sustituyo en nombre y descripcion por _ por espacio
            contenido[5] = contenido[5].replace("_", " ")
            contenido[4] = contenido[4].replace("_", " ")
            # Nos quedamos con los campos que nos interesan de la linea
            info = contenido[3:6]
            print(info[0])
            info[0] = self.__dateFormat(info[0])
            print(info[0])
            nTarea = contenido[1].split(':')
            nTarea = nTarea[0]
            newTarea = [nTarea] + info
            result.append(newTarea)
        return result

    # PRIVATE:   probar el funcionamiento del wrapper
    #           NOTA: no esta definido como metodo privado porque tenemos que usarlo
    #           para testear el wrapper, pero no lo consideramos parte de la API
    def test(self):
        self.iniciar()

        tarea = 0
        while tarea < 5:
            self.generalTask("01/02/04", "Tarea  " + str(tarea))
            tarea = tarea + 1

            # Medir tiempos
        t = time.time()
        self.generalTask("01/02/04", "GENERAL")
        t = time.time() - t
        print("NUEVA GENERAL: " + str(t))

        t = time.time()
        r = self.viewGeneralTask()
        t = time.time() - t
        print("LISTAR GENERALES: " + str(t))

        t = time.time()
        self.specificTask("01/10/04", "especifica", "soy prueba de especifica")
        t = time.time() - t
        print("NUEVA ESPECIFICA: " + str(t))

        t = time.time()
        s = self.viewSpecificTask()
        t = time.time() - t
        print("LISTAR ESPECIFICAS: " + str(t))

        print(r)
        print("------------------------------------")
        print(s)

        time.sleep(2)
        self.exitMainFrame()

    #########################################################################################
    #   PRIVATE METHODS
    #########################################################################################

    # PUBLIC:    iniciar la conexion, iniciar sesion y ejecutar tareas.c
    def iniciar(self):
        self.em.connect(self.__mainframe)
        self.em.wait_for_field()
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.em.fill_field(3, 18, self.__grupo, 8)
        self.em.fill_field(5, 18, self.__password, 8)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.em.fill_field(3, 15, 'tareas.c', 8)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        # Necesario porque la ejecucion de tareas tarda un poco
        time.sleep(self.timeout_iniciar)

    # PUBLIC:    termina la ejecucion de tareas.c y vuelve al menu principal del
    #           mainframe. 
    #           Si salimos -> perdemos las tareas almacenadas (no persistente)
    def exitMainFrame(self):
        self.__isMore()
        self.em.send_string('3', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)
        self.em.terminate()

    # PUBLIC:    INTRODUCIR NUEVA TAREA GENERAL
    #   IN:     fecha, descripcion
    #   OUT:    nada
    def generalTask(self, date, description):
        date = self.__fechasTruncate(date)
        description = self.__descriptionTruncate(description)

        self.__assignTask()
        self.__isMore()
        self.em.send_string('1', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.__isMore()
        self.em.send_string(date, ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.__isMore()
        self.em.send_string(description, ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.__mainMenu()

    # PUBLIC:    INTRODUCIR NUEVA TAREA ESPECIFICA
    #   IN:     fecha, nombre, descripcion
    #   OUT:    nada
    def specificTask(self, date, name, description):
        date = self.__fechasTruncate(date)
        description = self.__descriptionTruncate(description)
        name = self.__nameTruncate(name)

        self.__assignTask()
        self.__isMore()
        self.em.send_string('2', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.__isMore()
        self.em.send_string(date, ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.__isMore()
        self.em.send_string(name, ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.__isMore()
        self.em.send_string(description, ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        self.__mainMenu()

    # PUBLIC:    LISTAR TAREAS GENERALES
    #   IN:     sin argumentos
    #   OUT:    lista de listas. Cada componente una tarea.
    #           [[nTarea1,fecha1,nombre1,des1],[nTarea2,fecha2,nombre2,des2]]
    #           Nota: nombreX sera la cadena vacia porque las tareas 
    #           generales carecen de descripcion
    def viewGeneralTask(self):
        self.__viewTask()
        self.__isMore()
        self.em.send_string('1', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)
        tareas = list()
        # simulamos do while
        while True:
            tareas = self.__readTask(tareas, "GENERAL")
            more = self.em.string_get(43, 71, 4)
            if more != "More":
                break
            else:
                self.em.send_enter()
                self.em.wait_for_field()
                time.sleep(self.timeout)
        # Convertimos a lista de listas
        result = self.__listOfLists(tareas)
        self.__mainMenu()

        return result

    # PUBLIC:    LISTAR TAREAS ESPECIFICAS
    #   IN:     sin argumentos
    #   OUT:    lista de listas. Cada componente una tarea.
    #           [[nTarea1,fecha1,nombre1,des1],[nTarea2,fecha2,nombre2,des2]]
    def viewSpecificTask(self):
        self.__viewTask()
        self.__isMore()
        self.em.send_string('2', ypos=None, xpos=None)
        self.em.send_enter()
        self.em.wait_for_field()
        time.sleep(self.timeout)

        tareas = list()
        # simulamos do while
        while True:
            tareas = self.__readTask(tareas, "SPECIFIC")
            more = self.em.string_get(43, 71, 4)
            if more != "More":
                break
            else:
                self.em.send_enter()
                self.em.wait_for_field()
                time.sleep(self.timeout)
        # print(tareas)
        # Convertimos a lista de listas
        result = self.__listOfLists(tareas)

        self.__mainMenu()

        return result

# DESCOMENTAR PARA PROBAR WRAPPER
# wrapper = Wrapper()
# wrapper.test()
