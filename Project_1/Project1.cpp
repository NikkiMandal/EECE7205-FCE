#include <iostream>
#include <vector>
#include <climits>
using namespace std;

//define constant values for the maximum possible sizes of the input arrays and dynamic programming tables
const int MAX_N = 1000;  
const int MAX_M = 1000;

//initialize the first row and column for the dynamic programming 
int darr[MAX_N + 1][MAX_M + 1];
int G[MAX_M];

//dynamic programming for optimal solutions
vector<int> MaxMinGrouping(int A[], int N, int M) 
{
    for (int i = 1; i <= N; ++i) 
    {
        darr[i][1] = darr[i - 1][1] + A[i - 1];
    }

    for (int j = 2; j <= M; ++j) 
    {
        for (int i = j; i <= N; ++i) 
        {
            for (int k = j - 1; k < i; ++k) 
            {
                int temp = min(darr[k][j - 1], darr[i][1] - darr[k][1]);
                darr[i][j] = max(darr[i][j], temp);
            }
        }
    }

    int j = M, i = N;
    while (j > 0) 
    {
        for (int k = 0; k < i; ++k) 
        {
            if (darr[i][j] == darr[k][j - 1] || darr[i][j] == darr[i][1] - darr[k][1]) 
            {
                G[j - 1] = i - k;
                i = k;
                break;
            }
        }
        j--;
    }
    cout << "The Maximum minimum value of B is : " << darr[N][M] << endl;

    vector<int> result(G, G + M);
    return result;
}

int main() 
{
    int N;
    cout << "Enter the number of elements in array A: ";
    cin >> N;

    int A[MAX_N];
    cout << "Enter the elements of array A: ";
    for (int i = 0; i < N; ++i) 
    {
        cin >> A[i];
    }

    int M;
    cout << "Enter the number of groups (M) needed: ";
    cin >> M;

    vector<int> G = MaxMinGrouping(A, N, M);
    
    cout << "The Optimal grouping is: ";
    for (int group : G) 
    {
        cout << group << " ";
    }
    cout << "\nThe Elements in each group are:" << endl;

//ensure all the groups are non zero
    int nz = 0;
    for (int group : G) 
    {
        cout << "Group :" ;
        for (int i = 0; i < group; ++i) 
        {
            cout << A[nz++] << " ";
        }
        cout << endl;
    }
    return 0;
}
