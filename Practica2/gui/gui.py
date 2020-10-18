import PySimpleGUI as sg

sg.theme('Dark Grey 4')  # please make your windows colorful

NUM_COLS = 3
NUM_ROWS = 10
HEADINGS = ['Fecha', 'Nombre', 'Descripcion']
HEADINGS_WIDTHS = [10, 24, 32]
FONT = 'Helvetica 14'

# data = [[j for j in range(NUM_COLS)] for i in range(NUM_ROWS)]
data = [['', '', '']]
sin_tareas = True

col1 = [[sg.Text('Mis tareas')],
        [sg.Table(values=data,
                  key='-TABLE-',
                  auto_size_columns=False,
                  col_widths=HEADINGS_WIDTHS,
                  headings=HEADINGS,
                  num_rows=10,
                  pad=(20, 20),
                  row_height=30
                  )],
        ]
col2 = [[sg.Button('Nueva tarea')], [sg.Exit('Salir')]]
layout = [[sg.Col(col1, ), sg.Col(col2)]]
window = sg.Window('UnizarTasks', layout, font=FONT)


def new_task_win():
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
                    [sg.Input()],
                    [sg.Input()],
                    [sg.Input()]
                ]
            )
        ],
        [
            sg.Button('Añadir')
        ]
    ]
    return sg.Window('Nueva tarea', tarea_layout, font=FONT)


while True:
    event, values = window.read()
    if event == sg.WIN_CLOSED or event == 'Salir':
        break
    elif event == 'Nueva tarea':
        event2, values2 = new_task_win().read()
        fecha, nombre, descripcion = values2[0], values2[1], values2[2]
        if event2 == 'Añadir':
            # TODO Comprobar campos vacios y correctos
            if (sin_tareas):
                data.pop()
                sin_tareas = False
            data.append([fecha, nombre, descripcion])
            window['-TABLE-'].update(data)

window.close()
