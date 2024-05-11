import java.math.BigInteger;

public class PhraseEncoder {
    static String mapping = " ,." +
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
            "abcdefghijklmnopqrstuvwxyz";


    public static void main(String[] args) {
        String inputPhrase = "Hello, World!";
        BigInteger encodedInteger = encodePhrase(inputPhrase);

        System.out.println("Encoded Integer: " + encodedInteger);
    }

    public static BigInteger encodePhrase(String phrase) {

        BigInteger encodedInteger = BigInteger.ZERO;

        for (int i = 0; i < phrase.length(); i++) {
            char currentChar = phrase.charAt(i);
            int charCode = mapping.indexOf(currentChar);

            if (charCode != -1) {
                encodedInteger = encodedInteger.shiftLeft(6).add(BigInteger.valueOf(charCode));
            }
        }

        return encodedInteger;
    }
}
