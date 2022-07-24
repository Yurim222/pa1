/**********************************************************************
 * Copyright (c) 2021-2022
 *  Sang-Hoon Kim <sanghoonkim@ajou.ac.kr>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTIABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 **********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

/* To avoid security error on Visual Studio */
#define _CRT_SECURE_NO_WARNINGS
#pragma warning(disable : 4996)

/*====================================================================*/
/*          ****** DO NOT MODIFY ANYTHING FROM THIS LINE ******       */
#define MAX_NR_TOKENS   32   /* Maximum length of tokens in a command */
#define MAX_TOKEN_LEN   64   /* Maximum length of single token */
#define MAX_ASSEMBLY   256 /* Maximum length of assembly string */

typedef unsigned char bool;
#define true   1
#define false   0
/*          ****** DO NOT MODIFY ANYTHING UP TO THIS LINE ******      */
/*====================================================================*/


/***********************************************************************
 * translate()
 *
 * DESCRIPTION
 *   Translate assembly represented in @tokens[] into a MIPS instruction.
 *   This translate should support following 13 assembly commands
 *
 *    - add
 *    - addi
 *    - sub
 *    - and
 *    - andi
 *    - or
 *    - ori
 *    - nor
 *    - lw
 *    - sw
 *    - sll
 *    - srl
 *    - sra
 *    - beq
 *    - bne
 *
 * RETURN VALUE
 *   Return a 32-bit MIPS instruction
 *
 */
   struct R_Format{
      char *op_r;
      char *rs_r;
      char *rt_r;
      char *rd_r;
      char *shamt_r;
      char *funct_r;
   };

   struct I_Format{
      char *op_i;
      char *rs_i;
      char *rt_i;
      char *consta;
   };

   struct S_Format{
      char *op_s;
      char *rs_s;
      char *rt_s;
      char *shamt_s;
      char *funct_s;
   };


char *ten_to_bin(int num, int length)
   {  char *bin2=(char*) malloc(length+1); //변환한 비트를 담을 char 배열을 원하는 비트 크기만큼 만듦.
      int limit = length;
      int sign=1;// 부호가 양수인지 음수인지 구분.
      
      if(num<0) sign=-1; ////숫자가 음수인지 확인
        
      num = sign*num; /////sign이 음수인 경우 -1이어서 -1이 곱해지면 num은 양수 값이 나옴.

      while(num>0 && --limit>=0){
         if((num%2)==0) bin2[limit]='0';
         else bin2[limit] = '1';
         num = num>>1;
      }

      limit--;

      for(limit; limit>=0; limit--) bin2[limit]='0'; ///배열 앞의 빈 자리 비트들을 채워줌.

      //음수일 때
      if(sign==-1){
         for(int i=0; i<length; i++){
            if(bin2[i]=='0') bin2[i]='1';
            else bin2[i]='0';
         } // toggle

         for(int i=(length-1); i>=0; i--)
         {  if(bin2[i]=='1') bin2[i]='0';
            else{
            bin2[i]='1';
            break;
            }
         }

      }
      bin2[length]='\0';
      return bin2;
   }


   ///16진수를 2진수로_양수일 때 0x20
char *phex_to_bin(int num,int length)
{     char *bin2=(char*) malloc(length+1); //변환한 비트를 담을 char 배열을 원하는 비트 크기만큼 만듦.
      int limit = length;
      
      while(num>0 && --limit>=0){
         if((num%2)==0) bin2[limit]='0';
         else bin2[limit] = '1';
         num = num>>1;
      }

      limit--;

      for(limit; limit>=0; limit--) bin2[limit]='0';

      bin2[length]='\0';
      return bin2;
}

///16진수를 2진수로  - 음수버전
char *mhex_to_bin(int num, int length)
{     char *bin2=(char*) malloc(length+1); //변환한 비트를 담을 char 배열을 원하는 비트 크기만큼 만듦.
      int limit = length;

      while(num>0 && --limit>=0){
         if((num%2)==0) bin2[limit]='0';
         else bin2[limit] = '1';
         num = num>>1;
      }

      limit--;

      for(limit; limit>=0; limit--) bin2[limit]='0';
      
      for(int i=0; i<length; i++){
         if(bin2[i]=='0') bin2[i]='1';
         else bin2[i]='0';
      }

      for(int i=(length-1); i>=0; i--)
      {  if(bin2[i]=='1') bin2[i]='0';
         else{
         bin2[i]='1';
         break;
         }
      }
   
      bin2[length]='\0';
      return bin2;
}





static unsigned int translate(int nr_tokens, char *tokens[])
{
   int s_sham_data=0;
   int num=0;
   int count=0;
   char istr[64] = {'\0',};
   char *inst[] = {"sll", "srl", "sra", "addi", "andi", "ori", "lw", "sw", "beq", "bne", "add", "sub", "and", "or", "nor"};
    /////inst[0]~inst[2]: shift instruction /////// inst[3]~inst[9]: i-format instruction /////// inst[10]~inst[14]:r-format instruction

   char *registers[]={"zero", "at", "v0", "v1", "a0", "a1", "a2", "a3", "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
               "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "t8", "t9", "k1", "k2", "gp", "sp", "fp", "ra"};

   char *reg_bin[]={"00000","00001", "00010", "00011", "00100", "00101", "00110", "00111", "01000", "01001", "01010", "01011", "01100", "01101", "01110", 
               "01111", "10000", "10001", "10010", "10011", "10100", "10101", "10110", "10111", "11000", "11001", "11010", "11011", "11100", "11101",
               "11110", "11111"};


   struct R_Format R;
   struct I_Format I;
   struct S_Format S;

    for(int i=0; i<15; i++)
  {  if(strcmp(tokens[0],inst[i])==0){
      ///////shift일때//////
        if(i>=0&& i<3){
         
         S.op_s="000000";
         S.rs_s="00000";
         
         if(i==0) S.funct_s="000000";
         if(i==1) S.funct_s="000010";
         if(i==2) S.funct_s="000011";

         //register
         for(int j=1; j<3; j++)
            {
               for(int k=0; k<32; k++)
               {
                  if(strcmp(tokens[j],registers[k])==0)
                  {
                    count++;
                        switch (count) {
                        case 1: //tokens[1]일때
                        S.rt_s = reg_bin[k];
                        break;

                        case 2: //tokens[2]일때
                        S.rs_s = reg_bin[k];
                        break;

                        }
                    }
               }
            }
            //shamt
         
         
         if(strncmp(tokens[3],"-0x",3)==0){
            num=strtol((tokens[3]+1),NULL,16);
            S.shamt_s=mhex_to_bin(num,5);
         }
         else if(strncmp(tokens[3],"0x",2)==0){
            num=strtol(tokens[3],NULL,16);
            S.shamt_s=phex_to_bin(num,5);
         }
         else{
            num=strtol(tokens[3],NULL,10);
            S.shamt_s=ten_to_bin(num,5);
         }

         strcat(istr, S.op_s);
            strcat(istr, S.rs_s);
            strcat(istr, S.rt_s);
            // printf(istr,S.shamt_s,S.funct_s);
         //printf("i-str 확인 %s\n",istr);
         strcat(istr, S.shamt_s);
            strcat(istr, S.funct_s);
            return strtol(istr, NULL, 2);
           }

      //////i-format일 때!/////
        else if(i>2&&i<10){
           //printf("i-format instruction 처리 중! i 값은 : %d\n", i);
         if(i==3) I.op_i="001000";
         if(i==4) I.op_i="001100";
         if(i==5) I.op_i="001101";
         if(i==6) I.op_i="100011";
         if(i==7) I.op_i="101011";
         if(i==8) I.op_i="000100";
         if(i==9) I.op_i="000101";

         for(int j=1; j<3; j++)
            {
               for(int k=0; k<32; k++)
               {
                  if(strcmp(tokens[j],registers[k])==0)
                  {
                    count++;
                        switch (count) {
                        case 1: //tokens[1]일때
                        I.rt_i = reg_bin[k];
                        break;

                        case 2: //tokens[2]일때
                        I.rs_i = reg_bin[k];
                        break;

                        }
                    }
               }
            }
         
         if(strncmp(tokens[3],"-0x",3)==0){
            
            num=strtol((tokens[3]+1),NULL,16);
            I.consta=mhex_to_bin(num,16);
         }
         else if(strncmp(tokens[3],"0x",2)==0){
            
            num=strtol(tokens[3],NULL,16);
            I.consta=phex_to_bin(num,16);
         }
         else{
            
            num=strtol(tokens[3],NULL,10);
            I.consta=ten_to_bin(num,16);
         }
         strcat(istr, I.op_i);
            strcat(istr, I.rs_i);
            strcat(istr, I.rt_i);
            strcat(istr, I.consta);
            return strtol(istr, NULL, 2);
      }

      //////R-Format//////
        else if(i>9 && i<15){
            R.op_r = "000000"; //op_code는 0
            R.shamt_r = "00000";

            if(i==10) R.funct_r="100000";
            else if(i==11) R.funct_r="100010";
            else if(i==12) R.funct_r="100100";
            else if(i==13) R.funct_r="100101";
            else if(i==14) R.funct_r="100111";

            for(int j=1; j<4; j++)
            {
               for(int k=0; k<32; k++)
               {
                  if(strcmp(tokens[j],registers[k])==0)
                  {
                    count++;
                        switch (count) {
                        case 1: //tokens[1]일때
                        R.rd_r = reg_bin[k];
                        break;

                        case 2: //tokens[2]일때
                        R.rs_r = reg_bin[k];
                        break;

                        case 3: //tokens[3]일 때
                        R.rt_r = reg_bin[k];
                        break;
                        }
                    }
               }
            }

            strcat(istr, R.op_r);
            strcat(istr, R.rs_r);
            strcat(istr, R.rt_r);
            strcat(istr, R.rd_r);
            strcat(istr, R.shamt_r);
            strcat(istr, R.funct_r);
            return strtol(istr, NULL, 2);
        } ///// r-format instruction 완료
  }
    }
}


/***********************************************************************
 * parse_command()
 *
 * DESCRIPTION
 *   Parse @assembly, and put each assembly token into @tokens[] and the number
 *   of tokes into @nr_tokens. You may use this implemention or your own
 *   from PA0.
 *
 *   A assembly token is defined as a string without any whitespace (i.e., space
 *   and tab in this programming assignment). For exmaple,
 *     command = "  add t1   t2 s0 "
 *
 *   then, nr_tokens = 4, and tokens is
 *     tokens[0] = "add"
 *     tokens[1] = "t0"
 *     tokens[2] = "t1"
 *     tokens[3] = "s0"
 *
 *   You can assume that the characters in the input string are all lowercase
 *   for testing.
 *
 *
 * RETURN VALUE
 *   Return 0 after filling in @nr_tokens and @tokens[] properly
 *
 */
static int parse_command(char *assembly, int *nr_tokens, char *tokens[])
{
   char *curr = assembly;
   int token_started = false;
   *nr_tokens = 0;

   while (*curr != '\0') {  
      if (isspace(*curr)) {  
         *curr = '\0';
         token_started = false;
      } else {
         if (!token_started) {
            tokens[*nr_tokens] = curr;
            *nr_tokens += 1;
            token_started = true;
         }
      }
      curr++;
   }

   return 0;
}


/*====================================================================*/
/*          ****** DO NOT MODIFY ANYTHING BELOW THIS LINE ******      */

/***********************************************************************
 * The main function of this program.
 */
int main(int argc, char * const argv[])
{
   char assembly[MAX_ASSEMBLY] = { '\0' };
   FILE *input = stdin;

   if (argc > 1) {
      input = fopen(argv[1], "r");
      if (!input) {
         fprintf(stderr, "No input file %s\n", argv[0]);
         return EXIT_FAILURE;
      }
   }

   if (input == stdin) {
      printf("*********************************************************\n");
      printf("*          >> SCE212 MIPS translator  v0.10 <<          *\n");
      printf("*                                                       *\n");
      printf("*                                       .---.           *\n");
      printf("*                           .--------.  |___|           *\n");
      printf("*                           |.------.|  |=. |           *\n");
      printf("*                           || >>_  ||  |-- |           *\n");
      printf("*                           |'------'|  |   |           *\n");
      printf("*                           ')______('~~|___|           *\n");
      printf("*                                                       *\n");
      printf("*                                 Spring 2022           *\n");
      printf("*********************************************************\n\n");
      printf(">> ");
   }

   while (fgets(assembly, sizeof(assembly), input)) {
      char *tokens[MAX_NR_TOKENS] = { NULL };
      int nr_tokens = 0;
      unsigned int machine_code;

      for (size_t i = 0; i < strlen(assembly); i++) {
         assembly[i] = tolower(assembly[i]);
      }

      if (parse_command(assembly, &nr_tokens, tokens) < 0)
         continue;

      machine_code = translate(nr_tokens, tokens);

      fprintf(stderr, "0x%08x\n", machine_code);

      if (input == stdin) printf(">> ");
   }

   if (input != stdin) fclose(input);

   return EXIT_SUCCESS;
}