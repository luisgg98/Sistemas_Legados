import 'package:flutter/material.dart';

class Home extends StatefulWidget {
  @override
  _HomeState createState() => _HomeState();
}

class _HomeState extends State<Home> {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: Center(
        child: Column(
          children: [
            Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                Icon(Icons.movie),
                Text('Número de registros: '),
                Text('500')
              ],
            ),
            DataTable(
              columns: [
                DataColumn(label: Text('Nombre')),
                DataColumn(label: Text('Tipo')),
                DataColumn(label: Text('Cinta'))
              ],
              rows: [
                DataRow(cells: [
                  DataCell(Text('Soy Leyenda')),
                  DataCell(Text('Apocalipsis')),
                  DataCell(Text('A'))
                ]),
                DataRow(cells: [
                  DataCell(Text('El lobo de Wall Street')),
                  DataCell(Text('Finanzas')),
                  DataCell(Text('B'))
                ])
              ],
            )
          ],
        ),
      ),
    );
  }
}
