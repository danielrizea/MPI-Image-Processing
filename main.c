

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "mpi/mpi.h"
#include <math.h>

/*
 * 
 */
int** image;
int n; // nr de linii
int m; // nr de coloane
int maxC; // max in pgm
int minC; // min in pgm
int** localimage;

int** auxLocalImage;

int** bufferImage;

int stripSize;

MPI_Status stat;
int aux;

#define SMOOTH 0
#define BLUR 1
#define SHARPEN 2
#define MEAN_REMOVAL 3
#define EMBOSS 4



FILE *debug;


void trimiterePrimirePortiuneVecini(int numarProcese,int rank){

    int i,j;
    int nrRanduri = n/numarProcese;

    int randuriDeTrimis;
    int start,stop;

  
    
    if(rank == 0){

    //procesul 0 trimite portiunea care ii revine fiecarui proces
        for(i=1;i<numarProcese - 1;i++){

            start = i*nrRanduri;
            randuriDeTrimis = nrRanduri;
                
            // send cate randuri si randurile
         //  printf("Procesul %d trimite %d randuri catre procesul %d \n",rank,randuriDeTrimis,i);

           for(j=0;j<randuriDeTrimis;j++){
               
                MPI_Send(image[start+j],m,MPI_INT,i,1,MPI_COMM_WORLD);
                 }
            }



            start= (numarProcese-1)*nrRanduri;
            randuriDeTrimis = n - (numarProcese-1)*nrRanduri;
        //    printf("Procesul %d trimite %d randuri catre procesul %d \n",rank,randuriDeTrimis,i);
            // send cate randuri si randurile
            

           for(j=0;j<randuriDeTrimis;j++){

                MPI_Send(image[start+j],m,MPI_INT,i,1,MPI_COMM_WORLD);
            }
            
            // creare mod lucru local pentru procesul 0

               localimage = (int**)calloc(stripSize,sizeof(int*));
               
               for(i=0;i<stripSize;i++)
               {
                    localimage[i]=(int*)calloc(m,sizeof(int));
                   

               }
               for(i=0;i<stripSize;i++)
               for(j=0;j<m;j++)
                       localimage[i][j]=image[i][j];

            
                       
    }
    else
    {
    // pentru procesele ramase (cele care trebuiesc sa primeasca)

    
  // printf("procesul %d a primit %d randuri m = %d \n",rank,stripSize,m);

    localimage = (int**)calloc(stripSize,sizeof(int*));

    for(i=0;i<stripSize;i++)
        localimage[i]=(int*)calloc(m,sizeof(int));
    

    for(j=0;j<stripSize;j++)
        MPI_Recv(localimage[j],m,MPI_INT,0,1,MPI_COMM_WORLD,&stat);

   
    }


}

void determinareMinMax(int rank){

    int i,j;

    // pentru toate procesele , calcul minim
    int min=255;

    int max=0;

    for(i=0;i<stripSize;i++)
        for(j=0;j<m;j++)
        {
/*
            if(rank==3)
                printf("%d ",localimage[i][j]);
*/
            
            //printf("procesul %d compara %d\n",rank,min);
            if(min > localimage[i][j])
                min = localimage[i][j];

            if(max < localimage[i][j])
                max = localimage[i][j];
            

        }



   // printf("procesul %d are minimul %d\n",rank,min);

    // fiecare isi calculeaza minimul
    MPI_Barrier(MPI_COMM_WORLD);
    
    
    MPI_Allreduce(&min,&minC,1,MPI_INT,MPI_MIN,MPI_COMM_WORLD);

    MPI_Allreduce(&max,&maxC,1,MPI_INT,MPI_MAX,MPI_COMM_WORLD);
    

   // printf("Procesul %d are dupa reduce minimul %d  maximul %d\n",rank,minC,maxC);
   
}



void createAuxImage(){

    int i;
    int j;
    
    auxLocalImage = (int**)calloc(stripSize,sizeof(int*));

    for(i=0;i<stripSize;i++)
        auxLocalImage[i] = (int*)calloc(m,sizeof(int));



}
void ajustareContrast(int a,int b,int min, int max){

    int i,j;

    
    for(i=0;i<stripSize;i++)
        for(j=0;j<m;j++)
            auxLocalImage[i][j] = calculIntensitate(a,b,localimage[i][j],min,max);

    
            
}


int calculIntensitate(int a,int b,int orig, int min , int max){

    int result = (b-a)*(orig - min)/(max-min) + a;

    if(result < 0)
        result =0;

    if(result>255)
        result = 255;
    
}

int calculFiltru(int **filterArray, int factor, int offset,int **pixels){

    int i;
    int j;

    int result = 0;
    for(i=0;i<3;i++)
        for(j=0;j<3;j++)
            result += filterArray[i][j]*pixels[i][j];

     result = result / factor;

     result = result + offset;

     if(result<0)
         return 0;

     if(result>255)
         return 255;
     

     return result;

}

void aplicareFiltru(int filter,int rank,int noProcesses){


    int i,j;

    int **filterArray;
    int factor;
    int offset=0;


    filterArray = (int**)calloc(3,sizeof(int*));
    for(j=0;j<3;j++)
        filterArray[j] = (int*)calloc(3,sizeof(int));
    
    // calculul parametrilor filtrrului
    switch(filter){

        case SMOOTH : {
            filterArray[0][0] = 1; filterArray[0][1] =1; filterArray[0][2] = 1;
             filterArray[1][0] = 1; filterArray[1][1] =1; filterArray[1][2] = 1;
              filterArray[2][0] = 1; filterArray[2][1] =1; filterArray[2][2] = 1;
              factor = 9;
              offset = 0;

        } break;

        case BLUR : {
            filterArray[0][0] = 1; filterArray[0][1] =2; filterArray[0][2] = 1;
             filterArray[1][0] = 2; filterArray[1][1] =4; filterArray[1][2] = 2;
              filterArray[2][0] = 1; filterArray[2][1] =2; filterArray[2][2] = 1;
              factor = 16;
              offset = 0;

        } break;

        case SHARPEN : {
            filterArray[0][0] = 0; filterArray[0][1] =-2; filterArray[0][2] = 0;
             filterArray[1][0] = -2; filterArray[1][1] =11; filterArray[1][2] = -2;
              filterArray[2][0] = 0; filterArray[2][1] =-2; filterArray[2][2] = 0;
              factor = 3;
              offset = 0;

        } break;

        case MEAN_REMOVAL : {
            filterArray[0][0] = -1; filterArray[0][1] =-1; filterArray[0][2] = -1;
             filterArray[1][0] = -1; filterArray[1][1] =9; filterArray[1][2] = -1;
              filterArray[2][0] = -1; filterArray[2][1] =-1; filterArray[2][2] = -1;
              factor = 1;
              offset = 0;
        } break;

        case EMBOSS : {
            filterArray[0][0] = -1; filterArray[0][1] =0; filterArray[0][2] = -1;
             filterArray[1][0] = 0; filterArray[1][1] =4; filterArray[1][2] = 0;
              filterArray[2][0] = -1; filterArray[2][1] =0; filterArray[2][2] = -1;
              factor = 1;
              offset = 127;
        } break;

    }



/*
    if(rank==0)
    {

        printf("Aplica filtru %d %d m %d\n",filter,factor,m);

    }
*/
  


    // line buttom linia de la procesul mai mic ca si rang
    // line top linia de la procesul mai mare ca si rang

    int **matPixels;

        matPixels = (int**)calloc(3,sizeof(int*));
        for(i=0;i<3;i++)
            matPixels[i] = (int*)calloc(3,sizeof(int));

        //intai send dupa care receive

        int ** matAuxBord;

        matAuxBord=(int**)calloc(stripSize+2,sizeof(int*));

        for(i=0;i<stripSize+2;i++)
            matAuxBord[i] = (int*)calloc(m+2,sizeof(int));


        for(i=0;i<stripSize;i++)
            for(j=0;j<m;j++)
                matAuxBord[i+1][j+1] = localimage[i][j];
                

                MPI_Barrier(MPI_COMM_WORLD);
                
    if(rank==0){
        // primul proces

        int * lineButtom;
        lineButtom = (int*)calloc(m,sizeof(int));


        
                

        MPI_Send(localimage[stripSize-1],m,MPI_INT,rank+1,1,MPI_COMM_WORLD);

        MPI_Recv(lineButtom,m,MPI_INT,rank+1,1,MPI_COMM_WORLD,&stat);

        // calcul

        for(j=0;j<m;j++)
            matAuxBord[stripSize+1][j+1] =lineButtom[j];
        

        for(i=1;i<stripSize+1;i++)
            for(j=1;j<m+1;j++){

                matPixels[0][0] = matAuxBord[i-1][j-1];
                matPixels[0][1] = matAuxBord[i-1][j];
                matPixels[0][2] = matAuxBord[i-1][j+1];
                matPixels[1][0] = matAuxBord[i][j-1];
                matPixels[1][1] = matAuxBord[i][j];
                matPixels[1][2] = matAuxBord[i][j+1];
                matPixels[2][0] = matAuxBord[i+1][j-1];
                matPixels[2][1] = matAuxBord[i+1][j];
                matPixels[2][2] = matAuxBord[i+1][j+1];

                auxLocalImage[i-1][j-1] =  calculFiltru(filterArray,factor,offset,matPixels);
                
                
            }



    }
    else
        if(rank==noProcesses-1)
        {
            // ultimul proces


        int * lineTop;
        lineTop = (int*)calloc(m,sizeof(int));


        int **matPixels;

        matPixels = (int**)calloc(3,sizeof(int*));
        for(i=0;i<3;i++)
            matPixels[i] = (int*)calloc(3,sizeof(int));

        //intai send dupa care receive

        int ** matAuxBord;

        matAuxBord=(int**)calloc(stripSize+2,sizeof(int*));

        for(i=0;i<stripSize+2;i++)
            matAuxBord[i] = (int*)calloc(m+2,sizeof(int));


        for(i=0;i<stripSize;i++)
            for(j=0;j<m;j++)
                matAuxBord[i+1][j+1] = localimage[i][j];



        MPI_Send(localimage[0],m,MPI_INT,rank-1,1,MPI_COMM_WORLD);

        MPI_Recv(lineTop,m,MPI_INT,rank-1,1,MPI_COMM_WORLD,&stat);

        // calcul

        for(j=0;j<m;j++)
            matAuxBord[0][j+1] =lineTop[j];


        for(i=1;i<stripSize+1;i++)
            for(j=1;j<m+1;j++){

                matPixels[0][0] = matAuxBord[i-1][j-1];
                matPixels[0][1] = matAuxBord[i-1][j];
                matPixels[0][2] = matAuxBord[i-1][j+1];
                matPixels[1][0] = matAuxBord[i][j-1];
                matPixels[1][1] = matAuxBord[i][j];
                matPixels[1][2] = matAuxBord[i][j+1];
                matPixels[2][0] = matAuxBord[i+1][j-1];
                matPixels[2][1] = matAuxBord[i+1][j];
                matPixels[2][2] = matAuxBord[i+1][j+1];

                auxLocalImage[i-1][j-1] = calculFiltru(filterArray,factor,offset,matPixels);


            }


        }
        else
        {
            // procese intermediare, au ambii vecini

        int * lineTop;
        lineTop = (int*)calloc(m,sizeof(int));


         int * lineBottom;
        lineBottom = (int*)calloc(m,sizeof(int));

        int **matPixels;

        matPixels = (int**)calloc(3,sizeof(int*));
        for(i=0;i<3;i++)
            matPixels[i] = (int*)calloc(3,sizeof(int));

        //intai send dupa care receive

        int ** matAuxBord;

        matAuxBord=(int**)calloc(stripSize+2,sizeof(int*));

        for(i=0;i<stripSize+2;i++)
            matAuxBord[i] = (int*)calloc(m+2,sizeof(int));


        for(i=0;i<stripSize;i++)
            for(j=0;j<m;j++)
                matAuxBord[i+1][j+1] = localimage[i][j];



        MPI_Send(localimage[stripSize-1],m,MPI_INT,rank+1,1,MPI_COMM_WORLD);

        MPI_Send(localimage[0],m,MPI_INT,rank-1,1,MPI_COMM_WORLD);

        MPI_Recv(lineTop,m,MPI_INT,rank-1,1,MPI_COMM_WORLD,&stat);

        MPI_Recv(lineBottom,m,MPI_INT,rank+1,1,MPI_COMM_WORLD,&stat);
        
        // calcul

        for(j=0;j<m;j++)
            matAuxBord[stripSize+1][j+1] =lineBottom[j];


        for(j=0;j<m;j++)
            matAuxBord[0][j+1] =lineTop[j];

        for(i=1;i<stripSize+1;i++)
            for(j=1;j<m+1;j++){

                matPixels[0][0] = matAuxBord[i-1][j-1];
                matPixels[0][1] = matAuxBord[i-1][j];
                matPixels[0][2] = matAuxBord[i-1][j+1];
                matPixels[1][0] = matAuxBord[i][j-1];
                matPixels[1][1] = matAuxBord[i][j];
                matPixels[1][2] = matAuxBord[i][j+1];
                matPixels[2][0] = matAuxBord[i+1][j-1];
                matPixels[2][1] = matAuxBord[i+1][j];
                matPixels[2][2] = matAuxBord[i+1][j+1];

                auxLocalImage[i-1][j-1] = calculFiltru(filterArray,factor,offset,matPixels);


            }


        }



}
float calculPredictor(float a,float b,float c, int pa,int pb, int pc){

    return a*pa+b*pb+c*pc;


}


void calculEntropie(int a,int b,int c,int rank,int noProcesses,char *fileName){

    int i,j,k,l;


        //intai send dupa care receive
    MPI_Barrier(MPI_COMM_WORLD);


    int min=1000000;
    int max=-1000000;

    int minE;
    int maxE;


/*
    if(rank==0)
        printf("Calculul entropiei \n");
*/


        int ** matAuxBord;

        matAuxBord=(int**)calloc(stripSize+2,sizeof(int*));

        for(i=0;i<stripSize+2;i++)
            matAuxBord[i] = (int*)calloc(m+2,sizeof(int));


        for(i=0;i<stripSize;i++)
            for(j=0;j<m;j++)
                matAuxBord[i+1][j+1] = localimage[i][j];


               

    if(rank==0){
        // primul proces

        int * lineButtom;
        lineButtom = (int*)calloc(m,sizeof(int));





        MPI_Send(localimage[stripSize-1],m,MPI_INT,rank+1,1,MPI_COMM_WORLD);

        MPI_Recv(lineButtom,m,MPI_INT,rank+1,1,MPI_COMM_WORLD,&stat);

        // calcul
       
        for(j=0;j<m;j++)
            matAuxBord[stripSize+1][j+1] =lineButtom[j];


        for(i=1;i<stripSize+1;i++)
            for(j=1;j<m+1;j++){


                auxLocalImage[i-1][j-1] = matAuxBord[i][j]-ceil(calculPredictor(a,b,c,matAuxBord[i-1][j],matAuxBord[i-1][j-1],matAuxBord[i][j-1]));

                if(min > auxLocalImage[i-1][j-1])
                    min = auxLocalImage[i-1][j-1];

                if(max < auxLocalImage[i-1][j-1])
                    max = auxLocalImage[i-1][j-1];


            }

            // printf("procesul %d s-a deblocat \n",rank);

    }
    else
        if(rank==noProcesses-1)
        {
            // ultimul proces


        int * lineTop;
        lineTop = (int*)calloc(m,sizeof(int));


        int **matPixels;

        matPixels = (int**)calloc(3,sizeof(int*));
        for(i=0;i<3;i++)
            matPixels[i] = (int*)calloc(3,sizeof(int));

        //intai send dupa care receive

        int ** matAuxBord;

        matAuxBord=(int**)calloc(stripSize+2,sizeof(int*));

        for(i=0;i<stripSize+2;i++)
            matAuxBord[i] = (int*)calloc(m+2,sizeof(int));


        for(i=0;i<stripSize;i++)
            for(j=0;j<m;j++)
                matAuxBord[i+1][j+1] = localimage[i][j];



        MPI_Send(localimage[0],m,MPI_INT,rank-1,1,MPI_COMM_WORLD);

        MPI_Recv(lineTop,m,MPI_INT,rank-1,1,MPI_COMM_WORLD,&stat);
    
        // calcul

        for(j=0;j<m;j++)
            matAuxBord[0][j+1] =lineTop[j];


        for(i=1;i<stripSize+1;i++)
            for(j=1;j<m+1;j++){



                auxLocalImage[i-1][j-1] = matAuxBord[i][j]-ceil(calculPredictor(a,b,c,matAuxBord[i-1][j],matAuxBord[i-1][j-1],matAuxBord[i][j-1]));

                if(min > auxLocalImage[i-1][j-1])
                    min = auxLocalImage[i-1][j-1];

                if(max < auxLocalImage[i-1][j-1])
                    max = auxLocalImage[i-1][j-1];

            }
           // printf("procesul %d s-a deblocat \n",rank);

        }
        else
        {
            // procese intermediare, au ambii vecini

        int * lineTop;
        lineTop = (int*)calloc(m,sizeof(int));


         int * lineBottom;
        lineBottom = (int*)calloc(m,sizeof(int));

        int **matPixels;

        matPixels = (int**)calloc(3,sizeof(int*));
        for(i=0;i<3;i++)
            matPixels[i] = (int*)calloc(3,sizeof(int));

        //intai send dupa care receive

        int ** matAuxBord;

        matAuxBord=(int**)calloc(stripSize+2,sizeof(int*));

        for(i=0;i<stripSize+2;i++)
            matAuxBord[i] = (int*)calloc(m+2,sizeof(int));


        for(i=0;i<stripSize;i++)
            for(j=0;j<m;j++)
                matAuxBord[i+1][j+1] = localimage[i][j];



        MPI_Send(localimage[stripSize-1],m,MPI_INT,rank+1,1,MPI_COMM_WORLD);

        MPI_Send(localimage[0],m,MPI_INT,rank-1,1,MPI_COMM_WORLD);

        MPI_Recv(lineTop,m,MPI_INT,rank-1,1,MPI_COMM_WORLD,&stat);

        MPI_Recv(lineBottom,m,MPI_INT,rank+1,1,MPI_COMM_WORLD,&stat);

        // calcul

       

        for(j=0;j<m;j++)
            matAuxBord[stripSize+1][j+1] =lineBottom[j];


        for(j=0;j<m;j++)
            matAuxBord[0][j+1] =lineTop[j];

        for(i=1;i<stripSize+1;i++)
            for(j=1;j<m+1;j++){



                auxLocalImage[i-1][j-1] = matAuxBord[i][j]-ceil(calculPredictor(a,b,c,matAuxBord[i-1][j],matAuxBord[i-1][j-1],matAuxBord[i][j-1]));

                if(min > auxLocalImage[i-1][j-1])
                    min = auxLocalImage[i-1][j-1];

                if(max < auxLocalImage[i-1][j-1])
                    max = auxLocalImage[i-1][j-1];

            }
           //  printf("procesul %d s-a deblocat \n",rank);

        }


    
    // s-a calculat pana acum imaginea reziduala in auxLocalImage;


    MPI_Barrier(MPI_COMM_WORLD);

    

/*
    if(rank==0)
        printf("Imaginea reziduala a fost calculata");
*/


    int **imageEntropie;

    int* lineBuff;

    int* lineInd;
    if(rank==0)
    {



     lineBuff = (int*)calloc(m,sizeof(int));

     lineInd =  (int*)calloc(noProcesses,sizeof(int));



     imageEntropie = (int**)calloc(n,sizeof(int*));

     for(i=0;i<n;i++)
        imageEntropie[i] = (int*)calloc(m,sizeof(int));


     //procesul 0 isi completeaza in bufferImage randurile asignate lui
     for(i=0;i<stripSize;i++)
         memcpy(imageEntropie[i],auxLocalImage[i],sizeof(int)*m);




     for(i=stripSize;i<n;i++)
     {

         MPI_Recv(lineBuff,m,MPI_INT,MPI_ANY_SOURCE,1,MPI_COMM_WORLD,&stat);



         memcpy(imageEntropie[stat.MPI_SOURCE * (n/noProcesses) + lineInd[stat.MPI_SOURCE]], lineBuff, sizeof(int)*m);


         lineInd[stat.MPI_SOURCE]++;

     }
    //  printf("procesul %d s-a deblocat pt imagine \n",rank);


    }
    else
    {

        for(i=0;i<stripSize;i++)
            MPI_Send(auxLocalImage[i],m,MPI_INT,0,1,MPI_COMM_WORLD);




    }

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Allreduce(&min,&minE,1,MPI_INT,MPI_MIN,MPI_COMM_WORLD);

    MPI_Allreduce(&max,&maxE,1,MPI_INT,MPI_MAX,MPI_COMM_WORLD);


    if(rank==0){
        FILE * fout ;
        fout = fopen(fileName,"w");



        fprintf(fout,"%d %d\n",m,n);


        for(i=0;i<n;i++)
            for(j=0;j<m;j++)
                    fprintf(fout,"%d\n",imageEntropie[i][j]);
        fclose(fout);


    double entropie = 0;
    double pi = 0;

    int *frecvAparitii;



        frecvAparitii = (int*)calloc(maxE-minE+1,sizeof(int));

        for(i=0;i<n;i++)
            for(j=0;j<m;j++)
            {

               //printf(" pozitie %d \n",-minE+imageEntropie[i][j]);
                frecvAparitii[-minE+imageEntropie[i][j]]++;
            }


        for(i=0;i<maxE-minE+1;i++)
        {

            if(frecvAparitii[i] > 0){
            pi = (double)frecvAparitii[i]/(n*m);

        //    printf(" pi %d %d %lf\n",frecvAparitii[i],i,pi);


            entropie = entropie - pi*log2(pi);
            }
        }

     //   printf(" minE maxE %d %d ",minE,maxE);

        printf("Entropia pentru imaginea data este : %lf\n",entropie);
        
    }




}

void realizareNouaPoza(int rank,char *fileName,int noProcesses,char *pgmType, int maxRead){

    int i,j;

    MPI_Barrier(MPI_COMM_WORLD);

    int* lineBuff;

    int* lineInd;
    if(rank==0)
    {
     


     lineBuff = (int*)calloc(m,sizeof(int));

     lineInd =  (int*)calloc(noProcesses,sizeof(int));
     
     

     bufferImage = (int**)calloc(n,sizeof(int*));

     for(i=0;i<n;i++)
         bufferImage[i] = (int*)calloc(m,sizeof(int));


     //procesul 0 isi completeaza in bufferImage randurile asignate lui
     for(i=0;i<stripSize;i++)
         memcpy(bufferImage[i],auxLocalImage[i],sizeof(int)*m);
         

    

     for(i=stripSize;i<n;i++)
     {

         MPI_Recv(lineBuff,m,MPI_INT,MPI_ANY_SOURCE,1,MPI_COMM_WORLD,&stat);

         
         
         memcpy(bufferImage[stat.MPI_SOURCE * (n/noProcesses) + lineInd[stat.MPI_SOURCE]], lineBuff, sizeof(int)*m);
         

         lineInd[stat.MPI_SOURCE]++;

     }


    }
    else
    {
        
        for(i=0;i<stripSize;i++)
            MPI_Send(auxLocalImage[i],m,MPI_INT,0,1,MPI_COMM_WORLD);
                



    }

    MPI_Barrier(MPI_COMM_WORLD);

    if(rank==0){
        FILE * fout ;
        fout = fopen(fileName,"w");


        fprintf(fout,"%s\n",pgmType);
        fprintf(fout,"%d %d\n",m,n);
        fprintf(fout,"%d\n",maxRead);

        for(i=0;i<n;i++)
            for(j=0;j<m;j++)
                    fprintf(fout,"%d\n",bufferImage[i][j]);
        fclose(fout);

    }



}


int main(int argc, char** argv) {

    char *inputFile ;
    char *contrastOutputFile;
    int aContrast;
    int bContrast;
    char *filterType;
    char *filterOutputFile;
    char *residualImageFile;
    double aEntropie;
    double bEntropie;
    double cEntropie;

    char pgmType[10];
    char line[300];

    FILE * inFile;
    
  



    // aici este imaginea 
    int maxRead;
    
    int i;
    int j;

    int rc;
    int numtasks;
    int rank;
    int filter;


    if(argc < 10){
        printf("Nu s-au dat cele 10 argumente");
    return 1;
    }


    inputFile = strdup(argv[1]);
    aContrast = atoi(strdup(argv[2]));
    bContrast = atoi(strdup(argv[3]));
    contrastOutputFile = strdup(argv[4]);
    filterType = strdup(argv[5]);
    filterOutputFile = strdup(argv[6]);
    aEntropie = atof(strdup(argv[7]));
    bEntropie = atof(strdup(argv[8]));
    cEntropie = atof(strdup(argv[9]));
    residualImageFile = strdup(argv[10]);

    // undefined
    filter = 5;

    if(strcmp("smooth",filterType)==0)
        filter = SMOOTH;
    else
        if(strcmp("blur",filterType)==0)
            filter = BLUR;
        else
            if(strcmp("sharpen",filterType)==0)
            filter = SHARPEN;
            else
                if(strcmp("mean_removal",filterType)==0)
            filter = MEAN_REMOVAL;
                else
                    if(strcmp("emboss",filterType)==0)
            filter = EMBOSS;


                if(filter == 5)
                    printf("Filter not defined \n");
                



/*
    printf("Input file : %s\n",inputFile);
    printf(" : Contrast %d %d %s\n",aContrast,bContrast,contrastOutputFile);
    printf(" : Filter %s %s\n",filterType,filterOutputFile);
    printf(" : Entropie %d %d %d %s\n",aEntropie,bEntropie,cEntropie,residualImageFile);

*/

    // initializare mediu MPI
    rc = MPI_Init(&argc,&argv);
    if (rc != MPI_SUCCESS) {
     printf ("Error starting MPI program. Terminating.\n");
     MPI_Abort(MPI_COMM_WORLD, rc);
     }


    MPI_Comm_size(MPI_COMM_WORLD,&numtasks);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);





    if(rank == 0)
    {
        // doar procesul 0 citeste imaginea

        inFile = fopen(inputFile,"r");

        fscanf(inFile,"%s\n",pgmType);

        fgets(line,300,inFile);
      
        if(line[0]=='#'){
            // comentarii, sari peste

        //    printf("Gasit comentariu");
    
        fgets(line,300,inFile);
  

      
        }

        sscanf(line," %d %d \n",&m,&n);
        fscanf(inFile,"%d\n",&maxRead);
      
         printf("Procesul %d Input File PGM  %s %d %d %d\n",rank,pgmType,n,m,maxRead);

         // alocare spatiu pentru matrice
        image = (int**)calloc(n,sizeof(int*));
        for(i=0;i<n;i++)
            image[i]=(int*)calloc(m,sizeof(int));

        for(i=0;i<n;i++)
            for(j=0;j<m;j++)
                if(j<m-1)
                    fscanf(inFile,"%d ",&image[i][j]);
                else
                    fscanf(inFile,"%d\n",&image[i][j]);
                    
            //        afisareMatriceImagine();


           


         // dupa care procesul 0 trebuie sa distribui imaginea si celorlalti;
         
    }

   

    
   
    
    // procesul 0 trimite valoarea m si celorlalti
   MPI_Bcast(&m,1,MPI_INT,0,MPI_COMM_WORLD);

   // procesul 0 trimite valoarea n si celorlalti
   MPI_Bcast(&n,1,MPI_INT,0,MPI_COMM_WORLD);

   // procesul 0 trimite valoarea maxima si celorlalti
   MPI_Bcast(&maxRead,1,MPI_INT,0,MPI_COMM_WORLD);

 
    if(rank != numtasks -1)
        stripSize = n/numtasks;
    else
        stripSize = n - (n/numtasks)*(numtasks-1);



   MPI_Barrier(MPI_COMM_WORLD);

   trimiterePrimirePortiuneVecini(numtasks,rank);

  // printf("Procesul %d are %d randuri primite",rank,randuriPrimite);

   determinareMinMax(rank);

   // fiecare proces isi creeaza zona tampon unde isi pune prelucrarile intermediare
   createAuxImage();



   // procesul de ajustare contrast
   ajustareContrast(aContrast,bContrast,minC,maxC);

   // creare noua imagine pe baza datelor din auxLocalImage
   realizareNouaPoza(rank,contrastOutputFile,numtasks,pgmType,maxRead);

   // procesul de aplicare filtru
   aplicareFiltru(filter,rank,numtasks);

   realizareNouaPoza(rank,filterOutputFile,numtasks,pgmType,maxRead);



   calculEntropie(aEntropie,bEntropie,cEntropie,rank,numtasks,residualImageFile);


  // printf("Procesul %d are n m max %d %d %d\n",rank,n,m,max);
   



   // printf("Procesul %d\n",rank);


    MPI_Finalize();



    return (EXIT_SUCCESS);
}

