import 'dart:html';

import 'package:frontapp/models/program.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';

final String host = "http://80fd5d9c0c62.ngrok.io";

Future<List<String>> fetchProgramsName() async {
  final response = await http.get('$host/get_them_all/');
  List<String> lista = [];

  if (response.statusCode == 200) {
    List<dynamic> datos = jsonDecode(response.body);
    datos.forEach((element) {
      lista.add(element['nombre']);
    });
  }

  return lista;
}

Future<List<Program>> fetchProgramsByTape(String tape) async {
  final response = await http.get('$host/get_tape_all/?cinta=$tape');
  List<Program> listaProgramas = [];
  if (response.statusCode == 200) {
    List<dynamic> datos = jsonDecode(response.body);

    datos.forEach((element) {
      listaProgramas.add(Program(
        registro: element['registro'],
        nombre: element['nombre'],
        tipo: element['tipo'],
        cinta: element['cinta'],
      ));
    });
  }

  return listaProgramas;
}

Future<Program> fetchProgramByName(String name) async {
  final response = await http.get('$host/find_by_name/?program=${name}');
  Map<String, dynamic> datos = jsonDecode(response.body);

  return Program(
      registro: datos['registro'],
      nombre: datos['nombre'],
      tipo: datos['tipo'],
      cinta: datos['cinta']);
}
