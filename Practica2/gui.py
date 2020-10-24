from time import sleep

import PySimpleGUI as sg
import wrapper


class Gui:
    """
    Interfaz de la aplicación legada de tareas en un mainframe. Esta se conecta al mainframe y mediante un wrapper
    permite mostrar tareas y añadir otras nuevas
    """
    wr = wrapper.Wrapper()
    NUM_COLS = 3
    NUM_ROWS = 10
    HEADINGS = ['Fecha', 'Nombre', 'Descripcion']
    HEADINGS_WIDTHS = [10, 24, 32]
    FONT = 'Helvetica 14'

    def init(self):
        """ Pone en funcionamiento la aplicación. Conecta con el mainframe y muestra la nueva interfaz """
        # Iniciar wrapper e interfaz
        self.wr.iniciar()
        layout = self.__get_layout()
        window = sg.Window('UnizarTasks', layout, font=self.FONT)

        # Lectura de eventos en la app
        while True:
            event, values = window.read()
            if event == sg.WIN_CLOSED or event == 'Salir':
                break
            elif event == 'Nueva tarea':
                add_task_window = self.__new_task_win()
                while True:
                    event2, values2 = add_task_window.read()
                    if event2 == sg.WIN_CLOSED:
                        break
                    elif event2 == event2 == '-NOMBRE-' and len(values2['-NOMBRE-']) > 4:
                        add_task_window['-NOMBRE-'].update(values2['-NOMBRE-'][0:4])
                    elif event2 == '-DESCRIPCION-' and len(values2['-DESCRIPCION-']) > 12:
                        add_task_window['-DESCRIPCION-'].update(values2['-DESCRIPCION-'][0:12])
                    elif event2 == '-FECHA-':
                        if len(values2['-FECHA-']) > 10:
                            add_task_window['-FECHA-'].update(values2['-FECHA-'][0:10])
                        if len(values2['-FECHA-']) == 2:
                            add_task_window['-FECHA-'].update(values2['-FECHA-'] + '/')
                        if len(values2['-FECHA-']) == 5:
                            add_task_window['-FECHA-'].update(values2['-FECHA-'] + '/')
                    elif event2 == 'Añadir':
                        fecha, nombre, descripcion = values2['-FECHA-'], values2['-NOMBRE-'], values2['-DESCRIPCION-']
                        # Comprobar campos vacios y correctos
                        if len(descripcion) == 0 or len(fecha) < 10:
                            sg.popup("Introduce correctamente la fecha dd/mm/yyyy y descripcion")
                            continue
                        # Escribir en mainframe
                        self.__write_task(fecha, nombre, descripcion)
                        # Mostrar cambios
                        self.__reload_table(window)
                        break

                add_task_window.close()  # Fin de nueva tarea

        self.wr.exitMainFrame()
        window.close()

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

    def __get_layout(self):
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
        return layout

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
