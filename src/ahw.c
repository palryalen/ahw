#include <R.h>

void intersect(double *startInt, double *stopInt, int *nRows, 
               double *intTime, int *nIntTimes, double *newIntStart, 
               double *newIntStop, int *newOLine, int *newNRows) 
{ 
  double t0, t1;
  int i, j, k;
  k = 0; 
  j = 0;
  *newNRows = 0;
  for (i = 0; i < *nRows; i++)
  {
    t0 = startInt[i]; 
    t1 = stopInt[i];
    newIntStart[k] = t0;
    j  = 0; //Possible to speed up this one...
    while (intTime[j] <= t0 && j < *nIntTimes) 
      j++; 
    while (intTime[j] > t0 && intTime[j] < t1 && j < *nIntTimes)
    {
      newIntStop[k] = intTime[j];
      *newNRows = *newNRows + 1;
      newOLine[k] = i + 1;
      k++; 
      newIntStart[k] = intTime[j];
      j++;
    }
    newIntStop[k] = t1;
    *newNRows = *newNRows + 1;
    newOLine[k] = i + 1;
    k++;        
  }
}


void cumSumGroup(double * x, int * length, int * group)
{
  int i,j;
  j = 0;
  for (i = 0; i < *length; i=j+1)
  {  
    for (j = i+1; group[j] == group[i]; j++)
    { 
      x[j] = x[j] + x[j-1];
      if (j >= *length-1) break;
    }
  }
}


void cumProdGroup(double *x, int *length, int *group)
{
  int i,j;
  j = 0; 
  for (i = 0; i < *length; i=j+1)
  {  
    for (j = i+1; group[j] == group[i]; j++)
    { 
      x[j] = x[j] * x[j-1];
      if (j >= *length-1) break;
    }
  }
}


void rightShiftGroup(double *x, int *length, int *group, double *initValue, int *numberOfGroups)
{
  int i,j,k;
  i = *length-1;
  
  for(k = *numberOfGroups-1; k >= 0; k--)
  {
     for(j = i-1; (group[j] == group[i]) && (j >= 0); j--) 
     {
        x[j+1] = x[j]; 
     }
     x[j+1] = initValue[k];
     i=j;
  }
}
