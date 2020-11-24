import 'package:flutter/material.dart';
import 'package:frontapp/models/lista_programas.dart';
import 'package:frontapp/models/program.dart';
import 'package:frontapp/services/legacyapp.dart';
import 'package:provider/provider.dart';

class Buscador extends StatefulWidget {
  const Buscador({
    Key key,
  }) : super(key: key);

  @override
  _BuscadorState createState() => _BuscadorState();
}

class _BuscadorState extends State<Buscador> {
  @override
  Widget build(BuildContext context) {
    Size size = MediaQuery.of(context).size;
    String cintaABuscar;

    return Row(
      mainAxisAlignment: MainAxisAlignment.center,
      children: [
        Container(
          width: size.width * 0.25,
          child: TextField(
            decoration: InputDecoration(labelText: "Cinta"),
            onChanged: (value) {
              cintaABuscar = value;
            },
          ),
        ),
        SizedBox(
          width: 5,
        ),
        Container(
          child: ElevatedButton(
            child: Text("Buscar"),
            onPressed: () {
              List<Program> listaProgramas = fetchProgramsByTape(cintaABuscar);
              Provider.of<ListaProgramas>(context, listen: false)
                  .listaProgramas = listaProgramas;
            },
          ),
        )
      ],
    );
  }
}
