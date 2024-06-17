#include<iostream>
#include<vector>
#include<limits.h>
#include"Matrix.h"
#include<ctime>

using namespace std;
using namespace Numeric_lib;

struct Task {
	int taskNumber;  // task number
	bool isCloudTask;  // indicates whether the task is a cloud task
	double priority; // priority of the task
	int finishTimeLocal;  // finish time on local core
	int finishTimeSending; // finish time on sending
	int finishTimeCloud;   // finish time in the cloud
	int finishTimeReceiving;   // finish time on receiving
	int earliestStartLocal; // earliest start time on local core
	int earliestStartSending; // earliest start time on sending
	int earliestStartCloud;  // earliest start time in the cloud
	int earliestStartReceiving; // earliest start time on receiving
	int startTime; // actual start time
	int channel; // channel indicator (local core = 0,1,2, cloud=3)
	bool isExitTask;  // exit task indicator
	bool isEntryTask;  // entry task indicator
	int readyCountLocal;
	int readyCountSameChannel;
};


//Step one Phase one : Primary Assignment
void primaryAssignment(vector<Task>& tasks, Matrix<int, 2>& tm, int threshold)
{
	for (unsigned int i = 0; i < tm.dim1(); ++i)
	{
		tasks[i].taskNumber = i + 1;

		int minTime = tm(i, 0);

		for (unsigned int j = 0; j < tm.dim2(); ++j)
		{
			if (tm(i, j) < minTime)
			{
				minTime = tm(i, j);
			}
		}
		tasks[i].isCloudTask = (minTime > threshold) ? true : false;
	}
}

// Step one Phase two : Task Prioritizing 
void prioritize(vector<Task>& tasks, Matrix<int, 2>& timeMatrix, Matrix<int, 2>& dependencyMatrix, int threshold)
{
	unsigned int size = tasks.size();

	for (unsigned int i = 0; i < size; ++i)
	{
		int dependencyCount = 0;

		for (unsigned int j = 0; j < dependencyMatrix.dim2(); ++j)
		{
			if (dependencyMatrix(size - i - 1, j) == 1)
			{
				dependencyCount++;
			}
		}

		tasks[size - i - 1].isExitTask = (dependencyCount == 0) ? true : false;

		dependencyCount = 0;

		for (unsigned int j = 0; j < dependencyMatrix.dim2(); ++j)
		{
			if (dependencyMatrix(j, size - i - 1) == 1)
			{
				dependencyCount++;
			}
		}

		tasks[size - i - 1].isEntryTask = (dependencyCount == 0) ? true : false;

		double maxPriority = 0;
		double weight = 0;

		if (!(tasks[size - i - 1].isCloudTask))
		{
			for (unsigned int j = 0; j < timeMatrix.dim2(); ++j)
			{
				weight += timeMatrix(size - i - 1, j);
			}
			weight /= 3;
		}
		else
		{
			weight = threshold;
		}

		for (unsigned int j = 0; j < dependencyMatrix.dim2(); ++j)
		{
			if (dependencyMatrix(size - i - 1, j) == 1 && maxPriority < tasks[j].priority)
			{
				maxPriority = tasks[j].priority;
			}
		}

		tasks[size - i - 1].priority = weight + maxPriority;
	}
}


int find_biggest_priority(vector<Task>& ini)
{
	unsigned int i;
	int max = 0;
	for (i = 0; i < ini.size(); i++)
		if (ini[i].priority > ini[max].priority)
			max = i;
	return max;
}

// find the maximum in two numbers
int maximum(int& m, int& n)
{
	if (m >= n)
		return m;
	else
		return n;
}

// if local schedule, return earliestStartLocal
int d_earliestStartLocal(Task& vi, vector<Task>& S, Matrix<int, 2>& G)
{
	unsigned int i;
	unsigned int j;
	int max = 0;
	if (S.size() != 0)
	{
		for (i = 0; i < G.dim2(); i++)
			if (G(i, vi.taskNumber - 1) == 1)
				for (j = 0; j < S.size(); j++)
					if ((S[j].taskNumber == i + 1) && (max < maximum(S[j].finishTimeLocal, S[j].finishTimeReceiving)))
						max = maximum(S[j].finishTimeLocal, S[j].finishTimeReceiving);
	}
	return max;
}

// if cloud schedule, return earliestStartSending
int d_earliestStartSending(Task& vi, vector<Task>& S, Matrix<int, 2>& G)
{
	unsigned int i;
	unsigned int j;
	int max = 0;
	if (S.size() != 0)
	{
		for (i = 0; i < G.dim2(); i++)
			if (G(i, vi.taskNumber - 1) == 1)
				for (j = 0; j < S.size(); j++)
					if ((S[j].taskNumber == i + 1) && (max < maximum(S[j].finishTimeLocal, S[j].finishTimeSending)))
						max = maximum(S[j].finishTimeLocal, S[j].finishTimeSending);
	}
	return max;
}

// if cloud schedule, return RTC
int d_earliestStartCloud(Task& vi, vector<Task>& S, Matrix<int, 2>& G)
{
	unsigned int i;
	unsigned int j;
	int max = vi.finishTimeSending;
	if (S.size() != 0)
	{
		for (i = 0; i < G.dim2(); i++)
			if (G(i, vi.taskNumber - 1) == 1)
				for (j = 0; j < S.size(); j++)
					if ((S[j].taskNumber == i + 1) && (max < maximum(vi.finishTimeSending, S[j].finishTimeCloud)))
						max = maximum(vi.finishTimeSending, S[j].finishTimeCloud);
	}
	return max;
}

// if cloud schedule, return earliestStartReceiving
int d_earliestStartReceiving(Task& vi)
{
	return vi.finishTimeCloud;
}

// if local schedule, return the smallest finish time
int locals(Task& vi, vector<Task>& S, Matrix<int, 2>& G, Matrix<int, 2>& ta)
{
	vi.earliestStartLocal = d_earliestStartLocal(vi, S, G);
	unsigned int i;
	unsigned int j;
	int mint = INT_MAX;
	int ft;
	int max = 0; // find a local core's biggest finish time
	if (S.size() == 0)
	{
		for (i = 0; i < ta.dim2(); i++)
		{
			ft = ta(vi.taskNumber - 1, i);
			if (mint > ft)
			{
				mint = ft;
				vi.channel = i;
			}
		}
		return mint;
	}
	for (i = 0; i < ta.dim2(); i++)
	{
		ft = vi.earliestStartLocal + ta(vi.taskNumber - 1, i);
		max = 0;
		for (j = 0; j < S.size(); j++)
			if ((S[j].channel == i) && (max < S[j].finishTimeLocal))
				max = S[j].finishTimeLocal;
		if (max > vi.earliestStartLocal)
			ft = max + ta(vi.taskNumber - 1, i);
		if (mint > ft)
		{
			mint = ft;
			vi.channel = i;
		}
	}
	return mint;
}

// if cloud schedule, return the finish time
int clouds(Task& vi, vector<Task>& S, Matrix<int, 2>& G, int ts, int tc, int tr)
{
	vi.earliestStartSending = d_earliestStartSending(vi, S, G);
	unsigned int i;
	int maxs = 0;
	int t;
	int maxc = 0;
	int maxr = 0;
	int ft;
	t = ts + tc + tr;
	if (S.size() == 0)
	{
		vi.finishTimeSending = ts;
		vi.earliestStartCloud = ts;
		vi.finishTimeCloud = ts + tc;
		vi.earliestStartReceiving = ts + tc;
		return t;
	}
	for (i = 0; i < S.size(); i++)
		if (S[i].channel == 3)
			if (maxs < S[i].finishTimeSending)
				maxs = S[i].finishTimeSending;
	if (maxs > vi.earliestStartSending)
		vi.finishTimeSending = maxs + ts;
	else
		vi.finishTimeSending = vi.earliestStartSending + ts;
	vi.earliestStartCloud = d_earliestStartCloud(vi, S, G);
	for (i = 0; i < S.size(); i++)
		if (S[i].channel == 3)
			if (maxc < S[i].finishTimeCloud)
				maxc = S[i].finishTimeCloud;
	if (maxc > vi.earliestStartCloud)
		vi.finishTimeCloud = maxc + tc;
	else
		vi.finishTimeCloud = vi.earliestStartCloud + tc;
	vi.earliestStartReceiving = d_earliestStartReceiving(vi);
	for (i = 0; i < S.size(); i++)
		if (S[i].channel == 3)
			if (maxr < S[i].finishTimeReceiving)
				maxr = S[i].finishTimeReceiving;
	if (maxr > vi.earliestStartReceiving)
		ft = maxr + tr;
	else
		ft = vi.earliestStartReceiving + tr;
	return ft;
}

void initials(vector<Task>& S, vector<Task>& ini, Matrix<int, 2>& ta, Matrix<int, 2>& G, int ts, int tc, int tr)
{
	unsigned int i;
	int t;
	int maxp; // find the max priority in each iteration of ini
	int mint;  // find the minimum finish time of local
	int anot;  // perpare for another time (cloud)
	t = ts + tc + tr;
	for (i = 0; i < G.dim1(); i++)
	{
		maxp = find_biggest_priority(ini);
		if (!ini[maxp].isCloudTask)
		{
			mint = locals(ini[maxp], S, G, ta);
			anot = clouds(ini[maxp], S, G, ts, tc, tr);
			if (anot < mint)
			{
				ini[maxp].earliestStartLocal = 0;
				ini[maxp].finishTimeLocal = 0;
				ini[maxp].channel = 3;
				ini[maxp].finishTimeReceiving = anot;
				ini[maxp].startTime = anot - t;
			}
			else
			{
				ini[maxp].finishTimeCloud = 0;
				ini[maxp].finishTimeSending = 0;
				ini[maxp].earliestStartSending = 0;
				ini[maxp].earliestStartCloud = 0;
				ini[maxp].earliestStartReceiving = 0;
				ini[maxp].finishTimeReceiving = 0;
				ini[maxp].finishTimeLocal = mint;
				ini[maxp].startTime = mint - ta(ini[maxp].taskNumber - 1, ini[maxp].channel);
			}
		}
		else
		{
			ini[maxp].finishTimeLocal = 0;
			ini[maxp].earliestStartLocal = 0;
			ini[maxp].channel = 3;
			ini[maxp].finishTimeReceiving = clouds(ini[maxp], S, G, ts, tc, tr);
			ini[maxp].startTime = ini[maxp].finishTimeReceiving - t;
		}
		S.push_back(ini[maxp]);
		ini.erase(ini.begin() + maxp);
	}
}

// return a task's finish time
int find_ft(Task& vi)
{
	int max;
	max = maximum(vi.finishTimeLocal, vi.finishTimeReceiving);
	return max;
}

// print the sequence S
void prints(vector<Task>& S)
{
	unsigned int i;
	int k, m;
	for (i = 0; i < S.size(); i++)
	{
		k = 1 + S[i].channel;
		m = find_ft(S[i]);
		cout << "Task" << S[i].taskNumber << ": ";
		switch (S[i].channel)
		{
		case 0:
			cout << "Local Core number " << k << ", ";
			break;
		case 1:
			cout << "Local Core number " << k << ", ";
			break;
		case 2:
			cout << "Local Core number " << k << ", ";
			break;
		case 3:
			cout << "Cloud" << ", ";
			break;
		default:
			break;
		}
		cout << "The start time is: " << S[i].startTime << ",The finish time is: " << m << endl;
	}
}

// return the completion time of sequence S
int find_tcom(vector<Task>& S)
{
	unsigned int i;
	int max = 0;
	for (i = 0; i < S.size(); i++)
		if ((S[i].isExitTask) && (max < find_ft(S[i])))
			max = find_ft(S[i]);
	return max;
}

// return the total energy of the sequence S
double find_en(vector<Task>& S, int p1, int p2, int p3, double ps)
{
	unsigned int i;
	double ene = 0;
	for (i = 0; i < S.size(); i++)
	{
		switch (S[i].channel)
		{
		case 0:
			ene = ene + p1 * (find_ft(S[i]) - S[i].startTime);
			break;
		case 1:
			ene = ene + p2 * (find_ft(S[i]) - S[i].startTime);
			break;
		case 2:
			ene = ene + p3 * (find_ft(S[i]) - S[i].startTime);
			break;
		case 3:
			ene = ene + ps * (S[i].finishTimeSending - S[i].startTime);
			break;
		default:
			break;
		}
	}
	return ene;
}

//compute all the readyCountLocal in a sequence
void get_readyCountLocal(vector<Task>& S, Matrix<int, 2>& G)
{
	unsigned int i, j, k;
	int m;
	for (i = 0; i < S.size(); i++)
	{
		m = 0;
		for (j = 0; j < G.dim2(); j++)
			if (G(j, S[i].taskNumber - 1) == 1)
				for (k = 0; k < S.size(); k++)
					if (S[k].taskNumber == j + 1)
						m = m + 1;
		S[i].readyCountLocal = m;
	}
}

//compute all the readyCountSameChannel in a sequence
void get_readyCountSameChannel(vector<Task>& S)
{
	unsigned int i, j;
	int m;
	for (i = 0; i < S.size(); i++)
	{
		m = 0;
		for (j = 0; j < S.size(); j++)
			if ((S[i].channel == S[j].channel) && (S[j].startTime < S[i].startTime))
				m = m + 1;
		S[i].readyCountSameChannel = m;
	}
}

// local schedule task vi whose local core is confirmed
int localse(Task& vi, vector<Task>& SN, Matrix<int, 2>& G, Matrix<int, 2>& ta)
{
	vi.earliestStartLocal = d_earliestStartLocal(vi, SN, G);
	unsigned int i;
	int ft;
	int max = 0;
	if (SN.size() == 0)
		ft = vi.earliestStartLocal + ta(vi.taskNumber - 1, vi.channel);
	else
	{
		for (i = 0; i < SN.size(); i++)
			if ((SN[i].channel == vi.channel) && (max < SN[i].finishTimeLocal))
				max = SN[i].finishTimeLocal;
		if (max > vi.earliestStartLocal)
			ft = max + ta(vi.taskNumber - 1, vi.channel);
		else
			ft = vi.earliestStartLocal + ta(vi.taskNumber - 1, vi.channel);
	}
	return ft;
}

void kernel(vector<Task>& S, vector<Task>& SN, int ktar, Task vtar, Matrix<int, 2>& G, Matrix<int, 2>& ta, int ts, int tc, int tr)
{
	unsigned int i;
	int m;
	int t;
	t = ts + tc + tr;
	vector<Task>re;
	re = S;
	for (i = 0; i < re.size(); i++)
		if (vtar.taskNumber == re[i].taskNumber)
		{
			re[i].channel = ktar;
			if (ktar == 3)
			{
				re[i].finishTimeLocal = 0;
				re[i].earliestStartLocal = 0;
			}
		}
	while (re.size() != 0)
	{
		get_readyCountLocal(re, G);
		get_readyCountSameChannel(re);
		m = 0;
		while ((re[m].readyCountLocal != 0) && (re[m].readyCountSameChannel != 0))
			m = m + 1;
		if (re[m].channel == 3)
		{
			re[m].finishTimeReceiving = clouds(re[m], SN, G, ts, tc, tr);
			re[m].startTime = re[m].finishTimeReceiving - t;
		}
		else
		{
			re[m].finishTimeLocal = localse(re[m], SN, G, ta);
			re[m].startTime = re[m].finishTimeLocal - ta(re[m].taskNumber - 1, re[m].channel);
		}
		SN.push_back(re[m]);
		re.erase(re.begin() + m);
	}
}

void mcc(vector<Task>& S, Matrix<int, 2>& G, Matrix<int, 2>& ta, int ts, int tc, int tr, int p1, int p2, int p3, double ps, int tmax)
{
	unsigned int i, j;
	int tcom;
	int tcom2;
	int a;
	double en;
	double en1;
	double en2;
	double ratio1 = 0;
	double ratio2;
	vector<Task>SN;
	tcom = find_tcom(S);
	en = find_en(S, p1, p2, p3, ps);
	for (i = 0; i < S.size(); i++)
	{
		a = S[i].channel;
		if (S[i].channel != 3)
		{
			for (j = 0; j < 4; j++)
			{
				if (j != a)
				{
					SN.erase(SN.begin(), SN.end());
					en1 = find_en(S, p1, p2, p3, ps);
					kernel(S, SN, j, S[i], G, ta, ts, tc, tr);
					tcom2 = find_tcom(SN);
					en2 = find_en(SN, p1, p2, p3, ps);
					if ((en2 < en1) && (tcom >= tcom2))
						S = SN;
					else if ((en2 < en1) && (tcom2 <= tmax))
					{
						ratio2 = (en - en2) / (tcom2 - tcom);
						if (ratio2 > ratio1)
						{
							ratio1 = ratio2;
							S = SN;
						}
					}
				}
			}
		}
	}
}

void outerloop(vector<Task>& S, Matrix<int, 2>& G, Matrix<int, 2>& ta, int ts, int tc, int tr, int p1, int p2, int p3, double ps, int tmax)
{
	double en;
	double en1 = 0;
	en = find_en(S, p1, p2, p3, ps);
	while (en1 < en)
	{
		en = find_en(S, p1, p2, p3, ps);
		mcc(S, G, ta, ts, tc, tr, p1, p2, p3, ps, tmax);
		en1 = find_en(S, p1, p2, p3, ps);
	}
}

int main()
{
	int N1 = 10;  // the number of tasks
	int K = 3;   // the number of local cores
	int ts1 = 3, tc1 = 1, tr1 = 1;
	unsigned int i;
	int t1;
	int tmax1 = 27;
	int p11 = 1;
	int p12 = 2;
	int p13 = 4;
	double ps1 = 0.5;
	double rt;
	clock_t start, end;
	t1 = ts1 + tc1 + tr1;
	Matrix<int, 2>G1(N1, N1);
	Matrix<int, 2>ta1(N1, K);
	vector<Task>ini1(N1);
	vector<Task>S1;
	/*G1(0, 1) = 1;
	G1(0, 2) = 1;
	G1(0, 3) = 1;
	G1(0, 4) = 1;
	G1(0, 5) = 1;
	G1(1, 7) = 1;
	G1(1, 8) = 1;
	G1(2, 6) = 1;
	G1(3, 7) = 1;
	G1(3, 8) = 1;
	G1(4, 8) = 1;
	G1(5, 7) = 1;
	G1(6, 9) = 1;
	G1(7, 9) = 1;
	G1(8, 9) = 1;*/
	G1(0, 1) = 1;
	G1(0, 2) = 1;
	G1(0, 3) = 1;
	G1(0, 4) = 1;
	G1(0, 5) = 1;
	G1(1, 7) = 1;
	G1(1, 8) = 1;
	G1(2, 6) = 1;
	G1(2, 7) = 1; 
	G1(3, 8) = 1;
	G1(4, 7) = 1; 
	G1(5, 8) = 1;
	G1(6, 9) = 1;
	G1(7, 9) = 1;
	G1(8, 9) = 1;
	ta1(0, 0) = 9;
	ta1(0, 1) = 7;
	ta1(0, 2) = 5;
	ta1(1, 0) = 8;
	ta1(1, 1) = 6;
	ta1(1, 2) = 5;
	ta1(2, 0) = 6;
	ta1(2, 1) = 5;
	ta1(2, 2) = 4;
	ta1(3, 0) = 7;
	ta1(3, 1) = 5;
	ta1(3, 2) = 3;
	ta1(4, 0) = 5;
	ta1(4, 1) = 4;
	ta1(4, 2) = 2;
	ta1(5, 0) = 7;
	ta1(5, 1) = 6;
	ta1(5, 2) = 4;
	ta1(6, 0) = 8;
	ta1(6, 1) = 5;
	ta1(6, 2) = 3;
	ta1(7, 0) = 6;
	ta1(7, 1) = 4;
	ta1(7, 2) = 2;
	ta1(8, 0) = 5;
	ta1(8, 1) = 3;
	ta1(8, 2) = 2;
	ta1(9, 0) = 7;
	ta1(9, 1) = 4;
	ta1(9, 2) = 2;
	primaryAssignment(ini1, ta1, t1);
	prioritize(ini1, ta1, G1, t1);
	start = clock();
	initials(S1, ini1, ta1, G1, ts1, tc1, tr1);
	end = clock();
	cout << "Initial schedule:" << endl;
	prints(S1);
	rt = (double)(end - start) / (double)(CLOCKS_PER_SEC) * (double)(1000.000000);
	cout << " The total energy is: " << find_en(S1, p11, p12, p13, ps1) << endl;
	cout << " The completion time is: " << find_tcom(S1) << endl;
	cout << "The running time of initial schedule of Graph 1 is " << rt << " ms" << endl;
	start = clock();
	outerloop(S1, G1, ta1, ts1, tc1, tr1, p11, p12, p13, ps1, tmax1);
	end = clock();
	cout << "After Task Migration:" << endl;
	prints(S1);
	rt = (double)(end - start) / (double)(CLOCKS_PER_SEC) * (double)(1000.000000);
	cout << " The total energy is: " << find_en(S1, p11, p12, p13, ps1) << endl;
	cout << " The completion time is: " << find_tcom(S1) << endl;
	cout << "The running time of task migration of Graph 1 is " << rt << " ms" << endl;
	cout << endl;
	int N2 = 20;  // the number of tasks
	int ts2 = 3, tc2 = 2, tr2 = 1;
	int t2;
	int tmax2 = 27;
	int p21 = 2;
	int p22 = 3;
	int p23 = 5;
	double ps2 = 1.5;
	t2 = ts2 + tc2 + tr2;
	Matrix<int, 2>G2(N2, N2);
	Matrix<int, 2>ta2(N2, K);
	vector<Task>ini2(N2);
	vector<Task>S2;
	G2(0, 5) = 1;
	G2(1, 6) = 1;
	G2(2, 7) = 1;
	G2(3, 8) = 1;
	G2(4, 10) = 1;
	G2(9, 13) = 1;
	G2(5, 12) = 1;
	G2(6, 11) = 1;
	G2(7, 12) = 1;
	G2(8, 14) = 1;
	G2(10, 15) = 1;
	G2(12, 16) = 1;
	G2(13, 17) = 1;
	G2(14, 16) = 1;
	G2(15, 18) = 1;
	G2(17, 19) = 1;
	G2(16, 19) = 1;
	G2(11, 15) = 1;
	ta2(0, 0) = 9;
	ta2(0, 1) = 7;
	ta2(0, 2) = 5;
	ta2(1, 0) = 8;
	ta2(1, 1) = 6;
	ta2(1, 2) = 5;
	ta2(2, 0) = 6;
	ta2(2, 1) = 5;
	ta2(2, 2) = 4;
	ta2(3, 0) = 7;
	ta2(3, 1) = 5;
	ta2(3, 2) = 3;
	ta2(4, 0) = 5;
	ta2(4, 1) = 4;
	ta2(4, 2) = 2;
	ta2(5, 0) = 7;
	ta2(5, 1) = 6;
	ta2(5, 2) = 4;
	ta2(6, 0) = 8;
	ta2(6, 1) = 5;
	ta2(6, 2) = 3;
	ta2(7, 0) = 6;
	ta2(7, 1) = 4;
	ta2(7, 2) = 2;
	ta2(8, 0) = 5;
	ta2(8, 1) = 3;
	ta2(8, 2) = 2;
	ta2(9, 0) = 7;
	ta2(9, 1) = 4;
	ta2(9, 2) = 2;
	ta2(10, 0) = 7;
	ta2(10, 1) = 6;
	ta2(10, 2) = 5;
	ta2(11, 0) = 8;
	ta2(11, 1) = 7;
	ta2(11, 2) = 4;
	ta2(12, 0) = 9;
	ta2(12, 1) = 8;
	ta2(12, 2) = 6;
	ta2(13, 0) = 6;
	ta2(13, 1) = 6;
	ta2(13, 2) = 5;
	ta2(14, 0) = 4;
	ta2(14, 1) = 3;
	ta2(14, 2) = 3;
	ta2(15, 0) = 6;
	ta2(15, 1) = 5;
	ta2(15, 2) = 4;
	ta2(16, 0) = 8;
	ta2(16, 1) = 7;
	ta2(16, 2) = 6;
	ta2(17, 0) = 5;
	ta2(17, 1) = 4;
	ta2(17, 2) = 3;
	ta2(18, 0) = 5;
	ta2(18, 1) = 4;
	ta2(18, 2) = 2;
	ta2(19, 0) = 9;
	ta2(19, 1) = 8;
	ta2(19, 2) = 6;
	primaryAssignment(ini2, ta2, t2);
	prioritize(ini2, ta2, G2, t2);
	start = clock();
	initials(S2, ini2, ta2, G2, ts2, tc2, tr2);
	end = clock();
	cout << "Initial schedule:" << endl;
	prints(S2);
	rt = (double)(end - start) / (double)(CLOCKS_PER_SEC) * (double)(1000.000000);
	cout << " The total energy is: " << find_en(S2, p21, p22, p23, ps2) << endl;
	cout << " The completion time is: " << find_tcom(S2) << endl;
	cout << "The running time of initial schedule of Graph 2 is " << rt << " ms" << endl;
	start = clock();
	outerloop(S2, G2, ta2, ts2, tc2, tr2, p21, p22, p23, ps2, tmax2);
	end = clock();
	cout << "After all of Task Migration:" << endl;
	prints(S2);
	rt = (double)(end - start) / (double)(CLOCKS_PER_SEC) * (double)(1000.000000);
	cout << " The total energy is: " << find_en(S2, p21, p22, p23, ps2) << endl;
	cout << " The completion time is: " << find_tcom(S2) << endl;
	cout << "The running time of initial schedule of Graph 2 is " << rt << " ms" << endl;
	cout << endl;
}
