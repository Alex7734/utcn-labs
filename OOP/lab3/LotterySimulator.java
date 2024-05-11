import java.util.Random;

public class LotterySimulator {
    public static void main(String[] args) {
        int totalNumbers1 = 49;
        int numbersToChoose1 = 6;
        int totalNumbers2 = 40;
        int numbersToChoose2 = 5;

        int[] lotteryNumbers1 = generateRandomNumbers(totalNumbers1, numbersToChoose1);
        int[] lotteryNumbers2 = generateRandomNumbers(totalNumbers2, numbersToChoose2);

        System.out.println("Lottery Numbers (6 out of 49): ");
        printArray(lotteryNumbers1);

        System.out.println("Additional Numbers (5 out of 40): ");
        printArray(lotteryNumbers2);
    }

    private static int[] generateRandomNumbers(int totalNumbers, int numbersToChoose) {
        if (numbersToChoose > totalNumbers) {
            throw new IllegalArgumentException("Cannot choose more numbers than available.");
        }

        int[] lotteryNumbers = new int[numbersToChoose];
        Random random = new Random();

        for (int i = 0; i < numbersToChoose; i++) {
            int randomNum;
            do {
                randomNum = random.nextInt(totalNumbers) + 1;
            } while (contains(lotteryNumbers, randomNum));

            lotteryNumbers[i] = randomNum;
        }

        return lotteryNumbers;
    }

    private static boolean contains(int[] array, int value) {
        for (int num : array) {
            if (num == value) {
                return true;
            }
        }
        return false;
    }

    private static void printArray(int[] array) {
        for (int num : array) {
            System.out.print(num + " ");
        }
        System.out.println();
    }
}
