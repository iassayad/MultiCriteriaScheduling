
















typedef enum {false=0, true=1} bool;



/* SynDEx-6.7.0 (c)INRIA 2002, 15/6/2004 07:48:52
, application sampp, processor izard type=U */

#include <stdio.h> /* for printf */






#include <sys/types.h> /* for everybody */
#include <sys/ipc.h>   /* for semaphores and shared memory */
#include <sys/sem.h>   /* for semaphores: semget semctl */
#include <sys/shm.h>   /* for shared memory: shmget shmat shmdt shmctl */
#include <sys/wait.h>  /* for wait */

#define NSEMS 9 /* number of semaphores */

#define Semaphore_Thread_x 8
#define Semaphore_Thread_y 7
#define Sub_o__izard_y_empty 6
#define Sub_o__izard_y_full 5
#define input2_o__izard_x_empty 4
#define input2_o__izard_x_full 3
#define input1_o__izard_x_empty 2
#define input1_o__izard_x_full 1

int sems_id_; /* semaphores array identifier, initialized by main */
int shms_id_; /* shared memory identifier, initialized by main */

/* Precede(i) signals the i-th semaphore: */
void Precede(int i){
  struct sembuf s;
  s.sem_num=i; s.sem_op=+1; s.sem_flg=0;
  semop(sems_id_,&s,1);
}
/* Succede(i) waits until the i-th semaphore is signaled: */
void Succede(int i){
  struct sembuf s;
  s.sem_num=i; s.sem_op=-1; s.sem_flg=0;
  semop(sems_id_,&s,1);
}


/* Data buffers are grouped in the sp structure allocated in a memory */
/* shared between the main process and the communication process: */
typedef struct { /* data shared between main and com_thread */



int _input1_o[1];
#define input1_o sp->_input1_o
int _input2_o[1];
#define input2_o sp->_input2_o
int _Sub_o[1];
#define Sub_o sp->_Sub_o


/* media type = TCP media name = x */

} shmemory; /* end of shared data */
shmemory* sp; /* the mapped base address of the shared memory area */


/* support for workstations with different byte orders */
void brev2(int *data, int items) { /* b1b2 -> b2b1 */
 do { *data = *data<<8&0xff00 | *data>>8&0x00ff;
   data++;
 } while(--items);
}
void brev4(int *data, int items) { /* b1b2b3b4 -> b4b3b2b1 */
 do { *data = *data<<24 | *data<<8&0xff0000 | *data>>8&0xff00 | *data>>24&0xff;
   data++;
 } while(--items);
}
void brev8(int *data, int items) { /* 12345678 -> 78563412 */
  do { int t =
      *data<<24 | *data<<8&0xff0000 | *data>>8&0xff00 | *data>>24&0xff;
    data++; data[-1] =
      *data<<24 | *data<<8&0xff0000 | *data>>8&0xff00 | *data>>24&0xff;
    *data++ = t;
  } while(--items);
}

#ifdef DEBUG
#define INFO(x) printf x
#else
#define INFO(x)
#endif

#include <stdio.h>      /* for popen, pclose, sprintf */
#include <stdlib.h>     /* for system */
#include <string.h>     /* for strcpy */
#include <sys/socket.h> /* for everybody */
#include <netdb.h>      /* for gethostname and setsockopt */
#include <netinet/tcp.h>/* for TCP_NODELAY */
#include <netinet/in.h> /* for internet sockets */
#include <arpa/inet.h>  /* for inet_addr and inet_ntoa */
#include <unistd.h>     /* for read and write */
int com_thread_x(char* argv[]) { /* start of communication sequence */
#define MYID_x 1 /* my index on x bus */
#define NHOSTS_x 2 /* number of procrs connected on x bus */
  int sd[NHOSTS_x]; /* socket descriptors */
  struct sockaddr_in saddr; /* socket address */

  /* support for workstations with different byte orders */
#define BREVKEY 0x01234567
  int brev[NHOSTS_x]; /* byte-reverse when different byte orders */

  int len, n;
  char *pp; /* packet pointer */
  int i, j;
  struct hostpnx {
    u_short port;  /* TCP socket address  */
    char name[42]; /* internet hostname   */
    char exec[20]; /* executable filename */
  };
  
#define BOOTID 0 /* boot processor index */
  struct hostent *hp;
  struct hostpnx hpn;
  /* open a TCP socket for accepting connections */
  sd[MYID_x] = socket(PF_INET, SOCK_STREAM, 0);
  saddr.sin_family = AF_INET;
  saddr.sin_port = 0;  /* let the system allocate a TCP port */
  saddr.sin_addr.s_addr = INADDR_ANY; /* accept connexions from anywhere */
  bind(sd[MYID_x], (struct sockaddr*)&saddr, sizeof(saddr));
  listen(sd[MYID_x], 1);
  brev[MYID_x] = BREVKEY;
  /* IamNotRoot: get root address from command line arguments */
  hp = gethostbyname(argv[1]); /* get root name then address: */
  bcopy(hp->h_addr, (char*)&saddr.sin_addr.s_addr, sizeof(struct in_addr));
  saddr.sin_port = htons(atoi(argv[2])); /* get root listening port */
  saddr.sin_family = AF_INET;
  /* create a TCP socket and connect it to root */
  sd[BOOTID] = socket(PF_INET, SOCK_STREAM, 0);
  connect(sd[BOOTID], (struct sockaddr*)&saddr, sizeof(saddr));
  write(sd[BOOTID], (char*)(brev+MYID_x), 4); /* send my byte order */
  for(i=0; i<MYID_x; i++) if(i!=BOOTID) { /* for each of my servers: */
    saddr.sin_family = AF_INET;
    read(sd[BOOTID], &hpn, sizeof(hpn)); /* root sends server port and name */
    saddr.sin_port = hpn.port; /* retrieve port */
    hp = gethostbyname(hpn.name); /* retrieve name, then address: */
    bcopy(hp->h_addr, (char*)&saddr.sin_addr.s_addr, sizeof(struct in_addr));
    /* create a TCP socket and connect it to the server */
    sd[i] = socket(PF_INET, SOCK_STREAM, 0);
    connect(sd[i], (struct sockaddr*)&saddr, sizeof(saddr));
  }
  /* send to root my listening port address */
  len = sizeof(saddr);
  getsockname(sd[MYID_x], (struct sockaddr*)&saddr, &len);
  write(sd[BOOTID], (char*)&saddr.sin_port, 2);
  /* accept connexions from my clients */
  for(i=MYID_x+1; i<NHOSTS_x; i++) if(i!=BOOTID)
    sd[i] = accept(sd[MYID_x], (struct sockaddr*)&saddr, &len);
  /* receive from root all collected byte-orders */
  read(sd[BOOTID], (char*)brev, sizeof(brev));

  for(i=0; i<NHOSTS_x; i++) brev[i] ^= BREVKEY; /* compare byte-orders */
  /* prevent TCP from delaying uncomplete packets: */
  for(i=0, j=getprotobyname("tcp")->p_proto, n=1; i<NHOSTS_x; i++)
    setsockopt(sd[i], j, TCP_NODELAY, (char*)&n, sizeof(int));
  close(sd[MYID_x]); /* close my listening socket */
  /* End of network initializations */


  {int i; for(i=0; i<2; i++){ /* loop_2 */
    Succede(input1_o__izard_x_empty);
    for(pp=(char*)input1_o, len=1*sizeof(int); len!=0; pp+=n, len-=n)
    n = read(sd[0], pp, len);
    if(brev[0]) brev4((int*)input1_o, 1);
    Precede(input1_o__izard_x_full);
    Succede(input2_o__izard_x_empty);
    for(pp=(char*)input2_o, len=1*sizeof(int); len!=0; pp+=n, len-=n)
    n = read(sd[0], pp, len);
    if(brev[0]) brev4((int*)input2_o, 1);
    Precede(input2_o__izard_x_full);
  }} /* end loop_2 */
  
  return 0;
} /* end of com_thread_x */

/* media type = TCP media name = y */
int com_thread_y(char* argv[]) { /* start of communication sequence */
#define MYID_y -100 /* my index on y bus */
#define NHOSTS_y 3 /* number of procrs connected on y bus */
  int sd[NHOSTS_y]; /* socket descriptors */
  struct sockaddr_in saddr; /* socket address */

  /* support for workstations with different byte orders */
#define BREVKEY 0x01234567
  int brev[NHOSTS_y]; /* byte-reverse when different byte orders */

  int len, n;
  char *pp; /* packet pointer */
  int i, j;
  struct hostpnx {
    u_short port;  /* TCP socket address  */
    char name[42]; /* internet hostname   */
    char exec[20]; /* executable filename */
  };
  Precede(Sub_o__izard_y_empty);
  {int i; for(i=0; i<2; i++){ /* loop_4 */
    Succede(Sub_o__izard_y_full);
    write(sd[-100], (char*)Sub_o, 1*sizeof(int));
    Precede(Sub_o__izard_y_empty);
  }} /* end loop_4 */
  return 0;
} /* end of com_thread_y */

int main(int argc, char* argv[]) { /* for link with C runtime boot */
  /* setup IPC resources */
  sems_id_=semget(IPC_PRIVATE,NSEMS,0600|IPC_CREAT);
  if(sems_id_<0){perror("semget");return 1;}
  shms_id_=shmget(IPC_PRIVATE,sizeof(shmemory),0600|IPC_CREAT);
  if(shms_id_<0){perror("shmget");return 2;}
  sp=(shmemory*)shmat(shms_id_,NULL,0);
  if(sp==(shmemory*)-1){perror("shmat");return 3;}
  /* End of IPC initializations */
  if(fork()==0) return com_thread_x(argv);
  if(fork()==0) return com_thread_y(argv);
  Precede(input1_o__izard_x_empty);
  Precede(input2_o__izard_x_empty);
  {int i; for(i=0; i<2; i++){ /* loop_6 */
    Succede(input1_o__izard_x_full);
    Succede(input2_o__izard_x_full);
    Succede(Sub_o__izard_y_empty);
    Sub_o[0] = input1_o[0] - input2_o[0];
    Precede(input1_o__izard_x_empty);
    Precede(input2_o__izard_x_empty);
    Precede(Sub_o__izard_y_full);
  }} /* end loop_6 */
  { int status; wait(&status); } /* wait for com_thread_Semaphore_Thread_x() end */
  { int status; wait(&status); } /* wait for com_thread_Semaphore_Thread_y() end */
/* cleanup IPC resources */
  shmdt((char*)sp);
  shmctl(shms_id_,  IPC_RMID,0);
  semctl(sems_id_,0,IPC_RMID,0);

  return 0;
} /* end of main */


