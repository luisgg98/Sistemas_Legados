import 'package:flutter/material.dart';
import 'package:frontapp/models/lista_programas.dart';
import 'package:frontapp/models/program.dart';
import 'package:provider/provider.dart';

class ListaProgramasView extends StatelessWidget {
  const ListaProgramasView({
    Key key,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    List<Program> listaProgramas =
        Provider.of<ListaProgramas>(context, listen: true).listaProgramas;

    return ListView.builder(
      itemCount: listaProgramas == null ? 1 : listaProgramas.length + 1,
      itemBuilder: (context, index) {
        if (index == 0) {
          return ListTile(
            title: Row(
              children: [
                Expanded(
                  child: Text("Registro"),
                ),
                Expanded(
                  child: Text("Nombre"),
                ),
                Expanded(
                  child: Text("Tipo"),
                ),
                Expanded(
                  child: Text("Cinta"),
                ),
              ],
            ),
          );
        }
        index -= 1;

        return ListTile(
          title: Row(
            children: [
              Expanded(
                child: Text(listaProgramas[index].registro),
              ),
              Expanded(
                child: Text(listaProgramas[index].nombre),
              ),
              Expanded(
                child: Text(listaProgramas[index].tipo),
              ),
              Expanded(
                child: Text(listaProgramas[index].cinta),
              ),
            ],
          ),
        );
      },
    );
  }
}
