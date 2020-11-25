import 'package:flutter/material.dart';
import 'package:frontapp/models/lista_nombres_programas.dart';
import 'package:frontapp/models/lista_programas.dart';
import 'package:frontapp/services/legacyapp.dart';
import 'package:provider/provider.dart';

class ReloadButton extends StatefulWidget {
  const ReloadButton({
    Key key,
  }) : super(key: key);

  @override
  _ReloadButtonState createState() => _ReloadButtonState();
}

class _ReloadButtonState extends State<ReloadButton> {
  @override
  Widget build(BuildContext context) {
    return ElevatedButton(
      child: Row(
        children: [
          Icon(Icons.replay_outlined),
          SizedBox(
            width: 5,
          ),
          Text("Reload")
        ],
      ),
      onPressed: () {
        fetchProgramsName().then((value) {
          Provider.of<ListaNombresProgramas>(context, listen: false)
              .listaNombres = value;
        });
        Provider.of<ListaProgramas>(context, listen: false).deleteAll();
      },
    );
  }
}
