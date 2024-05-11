#include <iostream>
#include "Profiler.h"
using namespace std;

#define MAX_SIZE 10000
#define START_SIZE 100
#define STEP_SIZE 300
#define TEST_CASES 5
#define SELECTION_ASSIGN "Selection_Assignment"
#define SELECTION_COMPARE "Selection_Comparison"
#define BUBBLE_ASSIGN "Bubble_Assignment"
#define BUBBLE_COMPARE "Bubble_Comparison"
#define INSERT_ASSIGN "Insertion_Assignment"
#define INSERT_COMPARE "Insertion_Comparison"
#define SELECTION_SORT "Selection_Sort"
#define BUBBLE_SORT "Bubble_Sort"
#define INSERTION_SORT "Insertion_Sort"


/*
* @author Alex Mihoc
* @group 30425
*
* Assignment requirements, ex: Compare the three direct sorting algorithms
* (selection sort, bubble sort, insertion sort) on the average case, best case and worst case.
* As a bonus I also implemented the Binary Insertion Sort.
*
* The best case is when the array is already sorted.
* The worst case is when the array is sorted in reverse order.
* The average case is when the array is filled with random numbers.
*
* The clear winner is the Insertion Sort, followed by the Selection Sort and the Bubble Sort.
* The Binary Insertion Sort is better than the Insertion Sort because it uses binary search to find the proper position.
*
 */


Profiler profiler("directSortingAlgorithms");

void selectionSort(int array[], int size){
    Operation assign = profiler.createOperation(SELECTION_ASSIGN, size);
    Operation compare = profiler.createOperation(SELECTION_COMPARE, size);

    for (int i = 0; i < size - 1; i++){
        int min = i;
        for (int j = i + 1; j < size; j++){
            compare.count();
            if (array[j] < array[min]){
                min = j;
            }
        }
        if (min != i){
            swap(array[min], array[i]);
            assign.count(3);
        }
    }
};

void insertionSort(int array[], int size){
    Operation assign = profiler.createOperation(INSERT_ASSIGN, size);
    Operation compare = profiler.createOperation(INSERT_COMPARE, size);

    for (int i = 0; i < size; i++){
        assign.count(1);
        int insertElement = array[i];
        int j = i - 1;
        compare.count();
        while (j >= 0 && array[j] > insertElement){
            array[j + 1] = array[j];
            assign.count();
            j--;
        }
        array[j + 1] = insertElement;
        assign.count();
    }
}

void bubbleSort(int array[], int size){
    Operation assign = profiler.createOperation(BUBBLE_ASSIGN, size);
    Operation compare = profiler.createOperation(BUBBLE_COMPARE, size);

    for (int i = 0; i < size - 1; i++){
        bool swapped = false;
        for (int j = 0; j < size - 1; j++){
            compare.count();
            if (array[j] > array[j + 1]){
               swapped = true;
               swap(array[j], array[j + 1]);
               assign.count(3);
            }
        }
        if (!swapped) break;
    }
}

void binaryInsertionSort(int array[], int size) {
    for (int i = 1; i < size; i++) {
        int insertElement = array[i];
        int left = 0;
        int right = i - 1;

        while (left <= right) {
            int mid = left + (right - left) / 2;

            if (array[mid] > insertElement) {
                right = mid - 1;
            } else {
                left = mid + 1;
            }
        }

        for (int j = i - 1; j >= left; j--) {
            array[j + 1] = array[j];
        }

        array[left] = insertElement;
    }
}


void printStdOut(int array[], int size){
    for (int i = 0; i < size; i++){
        cout << array[i] << " ";
    }
}

void testAlgorithm(int array[], int size){
    FillRandomArray(array, size, 10, 50000, false);
    printStdOut(array, size);
    cout << "\n";
    selectionSort(array, size);
    printStdOut(array, size);
    cout << "\n";
}

void testBinaryInsertionSort(int array[], int size){
    FillRandomArray(array, size, 10, 50000, false);
    printStdOut(array, size);
    cout << "\n";
    binaryInsertionSort(array, size);
    printStdOut(array, size);
    cout << "\n";
}

void profilerReport(const char* compareGroupName, const char* assignGroupName, const char* sortGroupName){
    profiler.divideValues(SELECTION_COMPARE, TEST_CASES);
    profiler.divideValues(SELECTION_ASSIGN, TEST_CASES);
    profiler.addSeries(SELECTION_SORT, SELECTION_ASSIGN, SELECTION_COMPARE);

    profiler.divideValues(BUBBLE_ASSIGN, TEST_CASES);
    profiler.divideValues(BUBBLE_COMPARE, TEST_CASES);
    profiler.addSeries(BUBBLE_SORT, BUBBLE_ASSIGN, BUBBLE_COMPARE);

    profiler.divideValues(INSERT_ASSIGN, TEST_CASES);
    profiler.divideValues(INSERT_COMPARE, TEST_CASES);
    profiler.addSeries(INSERTION_SORT, INSERT_ASSIGN, INSERT_COMPARE);

    profiler.createGroup(compareGroupName, SELECTION_COMPARE, BUBBLE_COMPARE, INSERT_COMPARE);
    profiler.createGroup(assignGroupName, SELECTION_ASSIGN, BUBBLE_ASSIGN, INSERT_ASSIGN);
    profiler.createGroup(sortGroupName, SELECTION_SORT, BUBBLE_SORT, INSERTION_SORT);

}

void averageSortReport(){
    int arraySelectionSort[MAX_SIZE];
    int arrayBubbleSort[MAX_SIZE];
    int arrayInsertionSort[MAX_SIZE];

    for (int i = START_SIZE; i <= MAX_SIZE; i += STEP_SIZE){
        for (int m = 0; m < TEST_CASES; m++){
            FillRandomArray(arraySelectionSort, i, 1, 10000);
            selectionSort(arraySelectionSort, i);
            FillRandomArray(arrayBubbleSort, i, 1, 10000);
            bubbleSort(arrayBubbleSort, i);
            FillRandomArray(arrayInsertionSort, i, 1, 10000);
            insertionSort(arrayInsertionSort, i);
        }
    }

    profilerReport("Average Case Compare", "Average Case Assign", "Average Case Sort");
}

void bestCaseSortReport(){
    int arraySelectionSort[MAX_SIZE];
    int arrayBubbleSort[MAX_SIZE];
    int arrayInsertionSort[MAX_SIZE];

    for (int i = START_SIZE; i <= MAX_SIZE; i += STEP_SIZE){
        FillRandomArray(arraySelectionSort, i, 1, 10000, false, 1);
        selectionSort(arraySelectionSort, i);
        FillRandomArray(arrayBubbleSort, i, 1, 10000, false, 1);
        bubbleSort(arrayBubbleSort, i);
        FillRandomArray(arrayInsertionSort, i, 1, 10000, false, 1);
        insertionSort(arrayInsertionSort, i);
    }

    profilerReport("Best Case Compare", "Best Case Assign", "Best Case Sort");
}

void worstCaseSortReport(){
    int arraySelectionSort[MAX_SIZE];
    int arrayBubbleSort[MAX_SIZE];
    int arrayInsertionSort[MAX_SIZE];

    for (int i = START_SIZE; i <= MAX_SIZE; i += STEP_SIZE){
        FillRandomArray(arraySelectionSort, i, 1, 10000, false, 2);
        selectionSort(arraySelectionSort, i);
        FillRandomArray(arrayBubbleSort, i, 1, 10000, false, 2);
        bubbleSort(arrayBubbleSort, i);
        FillRandomArray(arrayInsertionSort, i, 1, 10000, false, 2);
        insertionSort(arrayInsertionSort, i);
    }

    profilerReport("Worst Case Compare", "Worst Case Assign", "Worst Case Sort");
}


int main() {

    int array[MAX_SIZE];
    int size = 10;
    testBinaryInsertionSort(array, size);

    averageSortReport();
    profiler.reset("Best Case");
    bestCaseSortReport();
    profiler.reset("Worst Case");
    worstCaseSortReport();
    profiler.showReport();

    return 0;
}


