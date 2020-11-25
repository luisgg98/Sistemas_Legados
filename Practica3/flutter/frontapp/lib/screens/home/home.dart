import 'package:flutter/material.dart';
import 'package:frontapp/models/lista_nombres_programas.dart';
import 'package:frontapp/models/lista_programas.dart';
import 'package:frontapp/screens/home/components/buscador.dart';
import 'package:frontapp/screens/home/components/lista_nombre_programas_view.dart';
import 'package:frontapp/screens/home/components/lista_programas_view.dart';
import 'package:frontapp/services/legacyapp.dart';
import 'package:provider/provider.dart';

import 'components/reload_button.dart';

class Home extends StatefulWidget {
  const Home({
    Key key,
  }) : super(key: key);

  @override
  _HomeState createState() => _HomeState();
}

class _HomeState extends State<Home> {
  @override
  Widget build(BuildContext context) {
    Size size = MediaQuery.of(context).size;

    return Scaffold(
        body: Container(
      height: size.height,
      width: size.width,
      child: Column(children: [
        Container(
          height: size.height * 0.1,
          width: size.width,
          child: Row(
            mainAxisAlignment: MainAxisAlignment.spaceAround,
            children: [
              ReloadButton(),
              Consumer<ListaNombresProgramas>(
                  builder: (context, listaNombres, child) =>
                      Text("Número de programas: ${listaNombres.numPrograms}")),
              Buscador(),
            ],
          ),
        ),
        Container(
            height: size.height * 0.9,
            width: size.width,
            child: Row(children: [
              Container(
                width: size.width * 0.3,
                child: ListaNombreProgramasView(),
              ),
              Container(width: size.width * 0.7, child: ListaProgramasView())
            ]))
      ]),
    ));
  }
}
