#include <iostream>
#include "Profiler.h"
using namespace std;

#define left(i) ((2 * (i)))
#define right(i) ((2 * (i)) + 1)
#define parent(i) ((i) / 2)
#define MAX_SIZE 10000
#define HEAP_SIZE 10
#define STEP_SIZE 300
#define TEST_CASES 1
#define MAX_HEAP 1
#define MIN_HEAP 0
#define TOP_DOWN 1
#define BOTTOM_UP 0

#define BOTTOM_UP_ASSIGN "Bottom_Up_Assign"
#define BOTTOM_UP_COMPARE "Bottom_Up_Comparison"
#define BOTTOM_UP_HEAPIFY "Bottom_Up_Heapify"

#define TOP_DOWN_ASSIGN "Top_Down_Assign"
#define TOP_DOWN_COMPARE "Top_Down_Comparison"
#define TOP_DOWN_HEAPIFY "Top_Down_Heapify"

#define WORST_CASE "Worst_Case"
#define AVERAGE_CASE "Average_Case"


Profiler profiler("Heap & Heap Sort");


void swap(int &a, int &b) {
    int temp = a;
    a = b;
    b = temp;
}

void heapifyBottomUp(int* array, int size, int index, Operation assign, Operation compare) {
    int currentElementIndex = index;

    int continueHeapifyingUp = 1;
    while (currentElementIndex > 0 && continueHeapifyingUp) {
        int parentIndex = parent(currentElementIndex);

        compare.count();
        if (array[currentElementIndex] > array[parentIndex]) {
            assign.count(3);
            swap(array[currentElementIndex], array[parentIndex]);
            currentElementIndex = parentIndex;
        } else {
            continueHeapifyingUp = 0;
        }
    }
}

void heapifyTopDown(int* array, int index, int size, Operation assign, Operation compare) {

    int parent = index;
    int left_child = left(index);
    int right_child = right(index);

    compare.count(1);
    if (left_child < size && array[left_child] > array[parent]) {
        parent = left_child;
    }

    compare.count(1);
    if (right_child < size && array[right_child] > array[parent]) {
        parent = right_child;
    }

    if (parent != index) {
        swap(array[index], array[parent]);
        assign.count(3);
        heapifyBottomUp(array, size, parent, assign, compare);
    }
}


void buildBottomUp(int* array, int size) {
    Operation assign = profiler.createOperation(BOTTOM_UP_ASSIGN, size);
    Operation compare = profiler.createOperation(BOTTOM_UP_COMPARE, size);

    for (int i = size / 2; i >= 0; i--) {
        heapifyBottomUp(array, size, i, assign, compare);
    }

    profiler.addSeries(BOTTOM_UP_HEAPIFY, BOTTOM_UP_ASSIGN, BOTTOM_UP_COMPARE);
}

void buildTopDown(int* array, int size){
    Operation assign = profiler.createOperation(TOP_DOWN_ASSIGN, size);
    Operation compare = profiler.createOperation(TOP_DOWN_COMPARE, size);

    for (int i = 1; i < size; i++){
        heapifyTopDown(array, i, size, assign, compare);
    }

    profiler.addSeries(TOP_DOWN_HEAPIFY, TOP_DOWN_ASSIGN, TOP_DOWN_COMPARE);
}

void heapsort(int* array, int size){
    Operation assign = profiler.createOperation(TOP_DOWN_ASSIGN, size);
    Operation compare = profiler.createOperation(TOP_DOWN_COMPARE, size);

    buildBottomUp(array, size);
    for (int i = size - 1; i > 0; i--){
        swap(array[0], array[i]);
        heapifyBottomUp(array, i, 0, assign, compare);
    }
}

void printHeap(int* array, int size) {
    for (int i = 0; i < size; i++) {
        cout << array[i] << " ";
    }
    cout << endl;
}

bool isHeap(const int* array, int size, int type) {
    const bool isMaxHeap = type == MAX_HEAP;
    for (int i = 0; i < size / 2; i++) {
        if (left(i) < size) {
            if ((isMaxHeap && array[left(i)] > array[i]) ||
                (!isMaxHeap && array[left(i)] < array[i])) {
                return false;
            }
        }

        if (right(i) < size) {
            if ((isMaxHeap && array[right(i)] > array[i]) ||
                (!isMaxHeap && array[right(i)] < array[i])) {
                return false;
            }
        }
    }

    return true;
}

void demonstrateHeapSortProcedure() {
    int heap[MAX_SIZE];
    FillRandomArray(heap, HEAP_SIZE, 10, 50000, false);

    cout << "Original Array:" << endl;
    printHeap(heap, HEAP_SIZE);

    heapsort(heap, HEAP_SIZE);

    cout << "Sorted Array:" << endl;
    printHeap(heap, HEAP_SIZE);
}

void demonstrateHeapifyProcedure(int type) {
    if (type != TOP_DOWN && type != BOTTOM_UP) {
        cout << "Invalid type." << endl;
        return;
    }

    int heap[MAX_SIZE];
    FillRandomArray(heap, HEAP_SIZE, 10, 50000, false);

    cout << "Original Array:" << endl;
    printHeap(heap, HEAP_SIZE);

    if (type == TOP_DOWN)
        buildTopDown(heap, HEAP_SIZE);
    else if (type == BOTTOM_UP)
        buildBottomUp(heap, HEAP_SIZE);

    cout << "Max-Heap:" << endl;
    printHeap(heap, HEAP_SIZE);
    if (isHeap(heap, HEAP_SIZE, MAX_HEAP))
        cout << "Heap is valid." << endl;
    else
        cout << "Heap is invalid." << endl;
}

void worstCase(){
    int arrayForBottomUp[MAX_SIZE], arrayForTopDown[MAX_SIZE];
    for (int currentSize = 100; currentSize <= MAX_SIZE; currentSize += STEP_SIZE){
        FillRandomArray(arrayForBottomUp, currentSize, 10, 50000, false, 1);
        memcpy(arrayForTopDown,arrayForBottomUp,currentSize*sizeof(int));
        buildBottomUp(arrayForBottomUp, currentSize);
        buildTopDown(arrayForBottomUp, currentSize);
    }

    profiler.createGroup(WORST_CASE, BOTTOM_UP_HEAPIFY, TOP_DOWN_HEAPIFY);
    profiler.showReport();
}

void averageCase(){
    int arrayForBottomUp[MAX_SIZE], arrayForTopDown[MAX_SIZE];
    for (int currentSize = 100; currentSize <= MAX_SIZE; currentSize += STEP_SIZE){
        FillRandomArray(arrayForBottomUp, currentSize, 10, 50000, false, 0);
        memcpy(arrayForTopDown,arrayForBottomUp,currentSize*sizeof(int));
        buildBottomUp(arrayForBottomUp, currentSize);
        buildTopDown(arrayForBottomUp, currentSize);
    }

    profiler.createGroup(AVERAGE_CASE, BOTTOM_UP_HEAPIFY, TOP_DOWN_HEAPIFY);
    profiler.showReport();
}

int main() {
    demonstrateHeapifyProcedure(BOTTOM_UP);
    printf("\n\n\n");
    demonstrateHeapSortProcedure();
    averageCase();
    return 0;
}
