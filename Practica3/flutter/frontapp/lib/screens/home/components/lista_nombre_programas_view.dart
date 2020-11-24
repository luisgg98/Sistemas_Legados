import 'package:flutter/material.dart';
import 'package:frontapp/models/lista_nombres_programas.dart';
import 'package:frontapp/models/lista_programas.dart';
import 'package:frontapp/models/program.dart';
import 'package:frontapp/services/legacyapp.dart';
import 'package:provider/provider.dart';

class ListaNombreProgramasView extends StatefulWidget {
  const ListaNombreProgramasView({
    Key key,
  }) : super(key: key);

  @override
  _ListaNombreProgramasViewState createState() =>
      _ListaNombreProgramasViewState();
}

class _ListaNombreProgramasViewState extends State<ListaNombreProgramasView> {
  @override
  Widget build(BuildContext context) {
    List<String> listaNombres =
        Provider.of<ListaNombresProgramas>(context, listen: true).listaNombres;

    return ListView.builder(
      itemCount: listaNombres == null ? 1 : listaNombres.length + 1,
      itemBuilder: (context, index) {
        if (index == 0) {
          return ListTile(title: Text('Nombre'));
        }
        index -= 1;

        return ListTile(
          title: Text(listaNombres[index]),
          onTap: () {
            List<Program> listaPrograma = [];
            listaPrograma.add(fetchProgramByName(listaNombres[index]));
            Provider.of<ListaProgramas>(context, listen: false).listaProgramas =
                listaPrograma;
          },
        );
      },
    );
  }
}
