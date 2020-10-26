from time import sleep

import PySimpleGUI as sg
import wrapper
import re
import unidecode


def fecha_correcta(fecha):
    match = re.match('^(0[1-9]|[12][0-9]|3[01])[/](0[1-9]|1[012])[/](19|20)\d\d$', fecha)
    if match is None:
        return False
    else:
        return True


def comprobar_campos_login(user, password):
    if len(user) == 0 or len(password) == 0:
        return False
    else:
        return True


class Gui:
    """
    Interfaz de la aplicación legada de tareas en un mainframe. Esta se conecta al mainframe y
    mediante un wrapper permite mostrar tareas y añadir otras nuevas
    """
    wr = wrapper.Wrapper()
    NUM_COLS = 3
    NUM_ROWS = 10
    HEADINGS = ['Fecha', 'Nombre', 'Descripcion']
    HEADINGS_WIDTHS = [10, 24, 32]
    FONT = 'Helvetica 14'

    def init(self):
        """ 
        Pone en funcionamiento la aplicación. Conecta con el mainframe y muestra la nueva 
        interfaz 
        """
        # Iniciar wrapper e interfaz
        self.wr.iniciar()

        # Inicio de sesion
        login_win = self.__get_login_win()
        login_correcto = False
        while True:
            event, values = login_win.read()
            if event == sg.WIN_CLOSED:
                break
            elif event == 'Iniciar sesion':
                user, password = values['-USUARIO-'], values['-CONTRASEÑA-']
                if comprobar_campos_login(user, password):
                    if self.__login(user, password):
                        login_correcto = True
                        break
                    else:
                        login_win['-USUARIO-'].update('')
                        login_win['-CONTRASEÑA-'].update('')
                        sg.popup('ERROR: Usuario y/o contraseña incorrectos')
                else:
                    sg.popup('ERROR: Los campos no pueden ser vacios')
        login_win.close()

        if login_correcto:
            main_win = self.__get__main_win()

            # Lectura de eventos en la app
            while True:
                event, values = main_win.read()
                if event == sg.WIN_CLOSED or event == 'Salir':
                    break
                elif event == 'Nueva tarea':
                    add_task_main_win = self.__new_task_win()
                    while True:
                        event2, values2 = add_task_main_win.read()
                        if event2 == sg.WIN_CLOSED:
                            break
                        elif event2 == event2 == '-NOMBRE-' and len(values2['-NOMBRE-']) > 4:
                            add_task_main_win['-NOMBRE-'].update(values2['-NOMBRE-'][0:4])
                        elif event2 == '-DESCRIPCION-' and len(values2['-DESCRIPCION-']) > 12:
                            add_task_main_win['-DESCRIPCION-'].update(values2['-DESCRIPCION-'][0:12])
                        elif event2 == '-FECHA-':
                            if len(values2['-FECHA-']) > 10:
                                add_task_main_win['-FECHA-'].update(values2['-FECHA-'][0:10])
                        elif event2 == 'Añadir':
                            fecha, nombre, descripcion = values2['-FECHA-'], values2['-NOMBRE-'], values2[
                                '-DESCRIPCION-']
                            # Comprobar campos vacios y correctos
                            if len(descripcion) == 0 or len(fecha) < 10 or not fecha_correcta(fecha):
                                sg.popup("Introduce correctamente la fecha dd/mm/yyyy y descripcion")
                                continue
                            nombre = unidecode.unidecode(nombre)
                            descripcion = unidecode.unidecode(descripcion)
                            # Escribir en mainframe
                            self.__write_task(fecha, nombre, descripcion)
                            # Mostrar cambios
                            self.__reload_table(main_win)
                            break

                    add_task_main_win.close()  # Fin de nueva tarea
            main_win.close()
        self.wr.exitMainFrame()

    def __login(self, user, password):
        return self.wr.login(user, password)

    def __write_task(self, fecha, nombre, descripcion):
        """ Escribir tarea en mainframe """
        if len(nombre) == 0:
            self.wr.generalTask(fecha, descripcion)
        else:
            self.wr.specificTask(fecha, nombre, descripcion)

    def __get_tasks(self):
        """ Obtener el conjunto de tareas del mainframe """
        general_tasks = self.wr.viewGeneralTask()
        specific_tasks = self.wr.viewSpecificTask()
        tasks = general_tasks + specific_tasks
        if len(tasks) == 0:
            tasks = [['', '', '']]  # Se necesita un campo mínimo
        else:
            tasks.sort(key=lambda x: x[0])  # Ordenar por número de tarea
            tasks = list(map(lambda x: x[1:], tasks))  # Eliminar numero de tarea
        return tasks

    def __get_login_win(self):
        login_layout = [
            [
                sg.Column(
                    [
                        [sg.Text('Usuario')],
                        [sg.Text('Contraseña')]
                    ]
                ),
                sg.Column(
                    [
                        [sg.Input(key='-USUARIO-')],
                        [sg.Input(key='-CONTRASEÑA-', password_char='*')],
                    ]
                )
            ],
            [
                sg.Button('Iniciar sesion')
            ]
        ]
        return sg.Window('Inicio de sesión', login_layout, font=self.FONT)

    def __get__main_win(self):
        """ Devuelve el layout general para crear la interfaz """
        data = self.__get_tasks()
        col1 = [[sg.Text('Mis tareas')],
                [sg.Table(values=data,
                          key='-TABLE-',
                          auto_size_columns=False,
                          col_widths=self.HEADINGS_WIDTHS,
                          headings=self.HEADINGS,
                          num_rows=10,
                          pad=(20, 20),
                          row_height=30
                          )],
                ]

        col2 = [[sg.Button('Nueva tarea')], [sg.Exit('Salir')]]
        layout = [[sg.Col(col1, ), sg.Col(col2)]]
        return sg.Window('UnizarTasks', layout, font=self.FONT)

    def __new_task_win(self):
        """ Devuelve Window correspondiente a la pantalla de creación de tareas"""
        tarea_layout = [
            [
                sg.Text("Introduce una nueva tarea")
            ],
            [
                sg.Column(
                    [
                        [sg.Text('Fecha')],
                        [sg.Text('Nombre')],
                        [sg.Text('Descripcion')]
                    ]
                ),
                sg.Column(
                    [
                        [sg.Input(key='-FECHA-', enable_events=True)],
                        [sg.Input(key='-NOMBRE-', enable_events=True)],
                        [sg.Input(key='-DESCRIPCION-', enable_events=True)]
                    ]
                )
            ],
            [
                sg.Button('Añadir')
            ]
        ]
        return sg.Window('Nueva tarea', tarea_layout, font=self.FONT)

    def __reload_table(self, window):
        # Obtener tareas
        data = self.__get_tasks()
        window['-TABLE-'].update(data)


gui = Gui()
gui.init()
