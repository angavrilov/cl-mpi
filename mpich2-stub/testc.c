
#include <stdio.h>
#include <stdlib.h>
int foo (int* a, char*** c)
{
  FILE *f;
  int i;
  char** cv = *c;
  f = fopen("temp.txt","w");
  fprintf(f,"a=%d,",*a);
  for (i=0;i<*a;i++) {
    fprintf(f,"arg[%d]=%s\n",i,cv[i]);
  }
  fclose(f);
  return 20+ *a;
}

int bogo_mpi_get_process (char* name, int* resultlen)
{
  sprintf(name,"BOGONODE");
  *resultlen=8;
  return 0;
}


int bogo_mpi_get_process1 (int* resultlen)
{
  /*  sprintf(name,"BOGONODE");*/
  *resultlen=8;
  return 0;
}

/*int MPI_Send( void *buf, int count, MPI_Datatype datatype, int dest, 
              int tag, MPI_Comm comm )
*/

int bogo_mpi_send(void *buf, int count, int datatype, int dest, int tag, int comm)
{
  /* checks send-string interface.
     writes the received string to file */
  FILE *f;
  int i;
  f = fopen("mpi_send_redeived.txt","w");
  fprintf(f,"sent to %d, tag=%d, comm=%d, count=%d,",dest, tag, comm, count);
  /* print buffer as string of characters (non-null terminated) */
  for (i=0;i<count;i++) {
    fprintf(f,"%c",((char*)buf)[i]);
  }
  fprintf(f,"\n");
  fclose(f);
  return 0;
}


int bogo_mpi_send1(void *buf, int count, int datatype)
{
  /* checks send-string interface.
     writes the received string to file */
  FILE *f;
  int i;
  char* cbuf  = (char*)buf;
  f = fopen("mpi_send_redeived.txt","w");
  fprintf(f,"count=%d,\n",count);
  /* print buffer as string of characters (non-null terminated) */
  for (i=0;i<count;i++) {
    fprintf(f,"%c",cbuf[i]);
  }
  fprintf(f,"\n");


  fclose(f);
  return 0;
}




typedef struct { 
    int count;
    int MPI_SOURCE;
    int MPI_TAG;
    int MPI_ERROR;
} bogo_MPI_Status;


int bogo_mpi_recv(void *buf, int count, int datatype, int source, int tag, int comm, bogo_MPI_Status *status)
{
  /* Note: count is the max size
     checks recv-string interface.
     test that status struct is filled properly
  */
  FILE *f;
  int i;
  char* cbuf = (char*)buf;
  sprintf(cbuf,"Hello, This is a test!");

  status->count = 22;
  status->MPI_SOURCE = source;
  status->MPI_TAG = tag;
  status->MPI_ERROR = 0;

  f = fopen("bogo_mpi_rcv.txt","w");
  fprintf(f,"from %d, tag=%d, comm=%d, count=%d,",source, tag, comm, count);

  for (i=0;i<count;i++) {
    fprintf(f,"%c",cbuf[i]);
    }
  fprintf(f,"\n");
  fclose(f);

  return 0;
}
