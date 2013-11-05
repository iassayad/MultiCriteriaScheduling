
















typedef enum {false=0, true=1} bool;



/* SynDEx-6.7.0 (c)INRIA 2002, 25/6/2004 13:09:44
, application sammp, processor root type=U */

#include <stdio.h> /* for printf */






#include <sys/types.h> /* for everybody */
#include <sys/ipc.h>   /* for semaphores and shared memory */
#include <sys/sem.h>   /* for semaphores: semget semctl */
#include <sys/shm.h>   /* for shared memory: shmget shmat shmdt shmctl */
#include <sys/wait.h>  /* for wait */

#define NSEMS 8 /* number of semaphores */

#define Semaphore_Thread_y 7
#define input1_o__root_y_empty 6
#define input1_o__root_y_full 5
#define add_o__root_y_empty 4
#define add_o__root_y_full 3
#define input2_o__root_y_empty 2
#define input2_o__root_y_full 1

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



int _add_o[1];
#define add_o sp->_add_o
int _input1_o[1];
#define input1_o sp->_input1_o
int _input2_o[1];
#define input2_o sp->_input2_o


/* media type = TCP media name = y */

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
int com_thread_y(char* argv[]) { /* start of communication sequence */
#define MYID_y 0 /* my index on y bus */
#define NHOSTS_y 2 /* number of procrs connected on y bus */
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
    /* IamRoot: I start all other workstations */
  static struct hostpnx host[NHOSTS_y] = {  {0,"root","root"},
    {0,"louve","louve"}
  };
  char command[200];
  char CWD[80]; /* HOME-relative Current Working Directory */
  { FILE *home, *pwd; char c;
    home = popen("cd;/bin/pwd","r"); /* absolute home path, \n at end */
    pwd  = popen(   "/bin/pwd","r"); /* present working directory */
    while((c = getc(home)) == getc(pwd) && c != '\n');
    fscanf(pwd, "%79s", CWD);
    pclose(home); pclose(pwd);
    if(c != '\n'){
      system("echo \".=`/bin/pwd` not under HOME=`cd;/bin/pwd`\" >&2");
      exit(-1);
    }
  }

  /* open a TCP socket for accepting connections */
  sd[MYID_y] = socket(PF_INET, SOCK_STREAM, 0);
  saddr.sin_family = AF_INET;
  saddr.sin_port = 0;  /* let the system allocate a TCP port */
  saddr.sin_addr.s_addr = INADDR_ANY; /* accept connexions from anywhere */
  bind(sd[MYID_y], (struct sockaddr*)&saddr, sizeof(saddr));
  listen(sd[MYID_y], 1);
  brev[MYID_y] = BREVKEY;
  /* retrieve the address of the listening TCP socket */
  len = sizeof(saddr);
  getsockname(sd[MYID_y], (struct sockaddr*)&saddr, &len);
  host[MYID_y].port = saddr.sin_port;
  gethostname(host[MYID_y].name, sizeof(host[MYID_y].name));
  for(i=0; i<NHOSTS_y; i++) if(i!=MYID_y) { /* start each other workstation */
    sprintf(command, "sh -c '\
H=%s;test -x $H.mnt||(rsh $H ln -s /bin/true ./%s/$H.mnt;\
test -x $H.mnt||ln -s /bin/false $H.mnt);./$H.mnt'",
            host[i].name, CWD);
    if(system(command)) /* if remote host does not share (mount) HOME, */
      /* rcp executable in same directory (tar creates it if needed), */
      sprintf(command, "(cd;tar cf - %s/%s)|rsh %s tar xf -\\;",
              CWD, host[i].exec, host[i].name);
      /* otherwise executable is already available on remote host; */
    else sprintf(command, "rsh %s -n ", host[i].name);
    /* then rexec executable in same directory, with root name and port */
    sprintf(command+strlen(command), "cd %s\\;./%s %s %d",
            CWD, host[i].exec, host[MYID_y].name, ntohs(host[MYID_y].port));
    INFO(("%s\n", command)); /* trace the command line */
    if(fork()==0) return system(command); /* execute it */
    len = sizeof(saddr); /* wait for connexion: */
    sd[i] = accept(sd[MYID_y], (struct sockaddr*)&saddr, &len);
    read(sd[i], (char*)(brev+i), 4); /* receive byte-order */
    /* send all listening ports already collected */
    for(j=0; j<i; j++) if(j!=MYID_y) write(sd[i], &host[j], sizeof(host[j]));
    read(sd[i], (char*)&host[i].port, 2); /* receive listening port */
    INFO((">>> %s %d\n", host[i].name, ntohs(host[i].port)));
  }
  /* broadcast byte-orders */
  for(i=0; i<NHOSTS_y; i++) if(i!=MYID_y) write(sd[i], (char*)brev, sizeof(brev));

  for(i=0; i<NHOSTS_y; i++) brev[i] ^= BREVKEY; /* compare byte-orders */
  /* prevent TCP from delaying uncomplete packets: */
  for(i=0, j=getprotobyname("tcp")->p_proto, n=1; i<NHOSTS_y; i++)
    setsockopt(sd[i], j, TCP_NODELAY, (char*)&n, sizeof(int));
  close(sd[MYID_y]); /* close my listening socket */
  /* End of network initializations */

  Precede(input2_o__root_y_empty);
  Precede(input1_o__root_y_empty);
  {int i; for(i=0; i<4; i++){ /* loop_2 */
    Succede(input2_o__root_y_full);
    write(sd[1], (char*)input2_o, 1*sizeof(int));
    Precede(input2_o__root_y_empty);
    Succede(input1_o__root_y_full);
    write(sd[1], (char*)input1_o, 1*sizeof(int));
    Precede(input1_o__root_y_empty);
    Succede(add_o__root_y_empty);
    for(pp=(char*)add_o, len=1*sizeof(int); len!=0; pp+=n, len-=n)
    n = read(sd[1], pp, len);
    if(brev[1]) brev4((int*)add_o, 1);
    Precede(add_o__root_y_full);
  }} /* end loop_2 */
  
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
  if(fork()==0) return com_thread_y(argv);
  
      Precede(add_o__root_y_empty);
  {int i; for(i=0; i<4; i++){ /* loop_4 */
    Succede(input1_o__root_y_empty);
    printf("input1_o[%d]: \n",0);;scanf("%d",&input1_o[0]);printf("%s\n","\n");;
    Precede(input1_o__root_y_full);
    Succede(input2_o__root_y_empty);
    input2_o[0] = 10;
    Precede(input2_o__root_y_full);
    Succede(add_o__root_y_full);
    printf("add_o[%d] : %d\n",0,add_o[0]);;printf("%s\n","\n");;
    Precede(add_o__root_y_empty);
  }} /* end loop_4 */
        { int status; wait(&status); } /* wait for com_thread_Semaphore_Thread_y() end */
/* cleanup IPC resources */
  shmdt((char*)sp);
  shmctl(shms_id_,  IPC_RMID,0);
  semctl(sems_id_,0,IPC_RMID,0);

  return 0;
} /* end of main */


