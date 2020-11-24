import 'package:frontapp/models/program.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';

List<String> fetchProgramsName() {
  List<String> nombres = [];
  for (int i = 0; i < 200; i++) {
    nombres.add("nombre" + i.toString());
  }

  return nombres;
  /*
  final response =
      await http.get('');

  if (response.statusCode == 200) {
    // If the server did return a 200 OK response,
    // then parse the JSON.
    List programas = jsonDecode(response.body)['data'];
    List<Program> listaProgramas = [];
    programas.forEach((element) {
      listaProgramas.add(Program(
          registro: element['registro'],
          nombre: element['nombre'],
          tipo: element['tipo'],
          cinta: element['cinta']));
    });
  } else {
    // If the server did not return a 200 OK response,
    // then throw an exception.
    throw Exception('Failed to load programs');
  }

  */
}

List<Program> fetchProgramsByTape(String tape) {
  List<Program> programas = [];
  for (int i = 0; i < 200; i++) {
    programas.add(Program(
        registro: i.toString(),
        nombre: "nombre$i",
        tipo: "tipo$i",
        cinta: tape));
  }
  return programas;
  /*
  final response =
      await http.get('');

  if (response.statusCode == 200) {
    // If the server did return a 200 OK response,
    // then parse the JSON.
    List programas = jsonDecode(response.body)['data'];
    List<Program> listaProgramas = [];
    programas.forEach((element) {
      listaProgramas.add(Program(
          registro: element['registro'],
          nombre: element['nombre'],
          tipo: element['tipo'],
          cinta: element['cinta']));
    });
  } else {
    // If the server did not return a 200 OK response,
    // then throw an exception.
    throw Exception('Failed to load programs');
  }

  */
}

Program fetchProgramByName(String name) {
  return Program(
      registro: "Registro$name",
      nombre: name,
      tipo: "tipo$name",
      cinta: "cinta$name");
}
