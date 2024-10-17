class Etl {
  Map<String, int> transform(Map<String, List<String>> original) =>
      Map.fromEntries(original.entries.expand(
          (MapEntry<String, List<String>> entry) => entry.value.map(
              (String letter) => MapEntry(
                  letter.toLowerCase(), int.parse(entry.key, radix: 10)))));
}
