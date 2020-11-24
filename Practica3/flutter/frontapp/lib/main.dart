import 'package:flutter/material.dart';
import 'package:frontapp/models/lista_nombres_programas.dart';
import 'package:frontapp/models/lista_programas.dart';
import 'package:frontapp/screens/home/home.dart';
import 'package:provider/provider.dart';

void main() {
  runApp(MaterialApp(
    theme: ThemeData(
      // Define the default brightness and colors.
      brightness: Brightness.dark,
      primaryColor: Colors.lightBlue[800],
      accentColor: Colors.cyan[600],

      // Define the default font family.
      fontFamily: 'Georgia',
    ),
    home: MultiProvider(
      providers: [
        ChangeNotifierProvider(create: (context) => ListaNombresProgramas()),
        ChangeNotifierProvider(create: (context) => ListaProgramas())
      ],
      child: Home(),
    ),
  ));
}
