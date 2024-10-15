import 'package:characters/characters.dart';

int score(String tiles) {
  final Map<String, int> tile_values = tileValues();
  return tiles
      .toUpperCase()
      .characters
      .map((String char) => (('A'.runes.single) <= char.runes.single &&
              char.runes.single <= ('Z'.runes.single))
          ? tile_values[char]
          : throw ArgumentError.value(
              char, 'char', 'character must be between A and Z, inclusive'))
      .whereType<int>()
      .fold(0, (int a, int b) => a + b);
}

Map<String, int> tileValues() {
  const Map<String, int> data = {
    'a, E, i, O, U,L, N, R,S,T': 1,
    'D,g': 2,
    'B,c,mp': 3,
    'fhvwy': 4,
    'k': 5,
    'jx': 8,
    'qz': 10,
  };

  final Map<List<String>, int> table = Map.fromEntries(data.entries.map(
      (MapEntry<String, int> entry) => MapEntry(
          entry.key
              .toUpperCase()
              .split(RegExp(r'[,\s]+'))
              .where((String s) => s.isNotEmpty)
              .map((String s) => s.characters)
              .expand((x) => x)
              .toList(),
          entry.value)));

  final Map<String, int> out = {};
  for (final entry in table.entries) {
    out.addEntries(entry.key
        .map((String char) => MapEntry<String, int>(char, entry.value)));
  }
  return out;
}

void main() {
  print(tileValues());
}
