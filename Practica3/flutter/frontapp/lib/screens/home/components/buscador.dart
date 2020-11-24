import 'package:flutter/material.dart';

class Buscador extends StatelessWidget {
  const Buscador({
    Key key,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    Size size = MediaQuery.of(context).size;

    return Row(
      mainAxisAlignment: MainAxisAlignment.center,
      children: [
        Container(
          width: size.width * 0.25,
          child: TextField(decoration: InputDecoration(labelText: "Cinta")),
        ),
        SizedBox(
          width: 5,
        ),
        Container(
          child: ElevatedButton(
            child: Text("Buscar"),
            onPressed: () {},
          ),
        )
      ],
    );
  }
}
