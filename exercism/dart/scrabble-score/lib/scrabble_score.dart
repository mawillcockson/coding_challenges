int score(String tiles) {
  final Map<String, int> tile_values = tileValues();
  return tiles
      .toUpperCase()
      .runes
      .cast<int>()
      .map(String.fromCharCode)
      .map((String char) => tile_values.containsKey(char)
          ? tile_values[char]
          : throw ArgumentError.value(
              char, 'char', 'character must be between A and Z, inclusive'))
      .whereType<int>()
      // I don't like that substituting this with .reduce((a, b) => a + b)
      // doesn't produce a compile- or runtime error on an empty input
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

  final Iterable<MapEntry<List<String>, int>> charValues = data.entries.map(
      (MapEntry<String, int> entry) => MapEntry<List<String>, int>(
          entry.key
              .toUpperCase()
              .split(RegExp(r'[,\s]+'))
              .where((String s) => s.isNotEmpty)
              .expand(
                  (String s) => s.runes.cast<int>().map(String.fromCharCode))
              .toList(),
          entry.value));

  final Map<String, int> out = {};
  for (final entry in charValues) {
    out.addEntries(entry.key
        .map((String char) => MapEntry<String, int>(char, entry.value)));
  }
  return out;
}

void main() {
  print(tileValues());
  assert(score('a') == 1);
  assert(score('A') == 1);
  assert(score('f') == 4);
  assert(score('at') == 2);
  assert(score('zoo') == 12);
  assert(score('street') == 6);
  assert(score('quirky') == 22);
  assert(score('OxyphenButazone') == 41);
  assert(score('pinata') == 8);
  assert(score('') == 0);
  assert(score('abcdefghijklmnopqrstuvwxyz') == 87);
}
