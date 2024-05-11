import java.util.Scanner;

public class GoldbachConjectureChecker {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.print("Enter the lower bound (m): ");
        int m = scanner.nextInt();

        if (m < 4 || m % 2 != 0) {
            System.out.println("m must be greater than or equal to 4 and even.");
            return;
        }

        System.out.print("Enter the upper bound (n): ");
        int n = scanner.nextInt();

        if (n % 2 != 0) {
            System.out.println("n must be an even number. Using n-1.");
            n -= 1;
        }
        scanner.close();

        checkGoldbachConjectureInRange(m, n);
    }

    public static void checkGoldbachConjectureInRange(int m, int n) {
        for (int evenNum = m; evenNum <= n; evenNum += 2) {
            boolean found = false;

            for (int prime = 2; prime <= evenNum / 2; prime++) {
                if (isPrime(prime) && isPrime(evenNum - prime)) {
                    found = true;
                    System.out.println(evenNum + " = " + prime + " + " + (evenNum - prime));
                    break;
                }
            }

            if (!found) {
                System.out.println("Goldbach's conjecture is false for " + evenNum);
            }
        }
    }

    public static boolean isPrime(int num) {
        if (num <= 1) {
            return false;
        }
        if (num <= 3) {
            return true;
        }
        if (num % 2 == 0 || num % 3 == 0) {
            return false;
        }

        for (int i = 5; i * i <= num; i += 6) {
            if (num % i == 0 || num % (i + 2) == 0) {
                return false;
            }
        }

        return true;
    }
}
