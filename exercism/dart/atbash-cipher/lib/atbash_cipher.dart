class AtbashCipher {
  final Map<String, String> encipher;
  final Map<String, String> decipher;

  static AtbashCipher? _instance = null;

  factory AtbashCipher(
      {String alphabet = 'abcdefghijklmnopqrstuvwxyz',
      String cipher = 'zyxwvutsrqponmlkjihgfedcba'}) {
    final alphabetSet = alphabet.runes.toSet();
    final cipherSet = cipher.runes.toSet();
    assert(alphabetSet.containsAll(cipherSet) &&
        cipherSet.containsAll(alphabetSet));

    final Map<String, String> encipher = {};
    final Map<String, String> decipher = {};
    for (int i = 0; i < alphabet.length; i++) {
      encipher[alphabet[i]] = cipher[i];
      decipher[cipher[i]] = alphabet[i];
    }
    _instance ??= AtbashCipher._internal(
        Map.unmodifiable(encipher), Map.unmodifiable(decipher));
    return _instance!;
  }

  AtbashCipher._internal(this.encipher, this.decipher);

  String encode(String message) {
    final String swapped = message.splitMapJoin(RegExp(r'[\w0-9]'),
        onMatch: (Match m) {
          String letter = m[0]!.toLowerCase();
          return this.encipher[letter] ?? letter;
        },
        onNonMatch: (x) => '');

    Iterable<String> iter = swapped.runes.map(String.fromCharCode);

    List<String> chunked = [iter.take(5).join()];
    iter = iter.skip(5);
    while (iter.isNotEmpty) {
      final chunk = iter.take(5).join();
      chunked.add(chunk);
      iter = iter.skip(5);
    }

    return chunked.join(" ");
  }

  String decode(String cipher) => cipher
      .splitMapJoin(RegExp(r'[\w0-9]'),
          onMatch: (Match m) => m[0]!
              .runes
              .map(String.fromCharCode)
              .map((String s) => this.decipher[s] ?? s)
              .join(),
          onNonMatch: (x) => '')
      .replaceAll(r'\s+', '');
}

void main() {
  AtbashCipher atbashCipher = AtbashCipher();

  assert(atbashCipher.encode("yes").length != 0);
  assert(atbashCipher.encode("yes").length == 3);
  assert(atbashCipher.encode("yes") == "bvh");

  assert(atbashCipher.decode(" ") == "");
  print('should be exercism -> ${atbashCipher.decode("vcvix rhn")}');

  assert(atbashCipher.encode("OMG123,hello fkjw  zyx    ") ==
      "lnt12 3svoo lupqd abc");
  assert(atbashCipher.decode("lnt12 3svoo lupqd abc") == "omg123hellofkjwzyx");
}
