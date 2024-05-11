import java.math.BigInteger;

public class LotteryOddsCalculator {
    public static void main(String[] args) {
        int totalNumbers1 = 49;
        int numbersToChoose1 = 6;
        int totalNumbers2 = 40;
        int numbersToChoose2 = 5;

        BigInteger firstArgument = calculateCombinations(totalNumbers1, numbersToChoose1);
        BigInteger secondArgument = calculateCombinations(totalNumbers2, numbersToChoose2);
        BigInteger totalCombinations = firstArgument.multiply(secondArgument);

        System.out.println("Odds of winning: 1 in " + totalCombinations);
    }

    private static BigInteger calculateCombinations(int n, int k) {
        BigInteger numerator = factorial(n);
        BigInteger denominator = factorial(k).multiply(factorial(n - k));
        return numerator.divide(denominator);
    }

    private static BigInteger factorial(int n) {
        if (n < 0) {
            throw new IllegalArgumentException("Factorial is not defined for negative numbers");
        }
        BigInteger result = BigInteger.valueOf(1);
        for (int i = 1; i <= n; i++) {
            result = result.multiply(BigInteger.valueOf(i));
        }
        return result;
    }
}

