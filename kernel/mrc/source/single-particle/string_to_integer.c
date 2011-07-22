int  string_to_integer(char *st)
{
     int i,len,j,flag=1;
     int x, start=0, sign=1;
     j=0;
     x=0;
     
     len=strlen(st);
     if(st[0]=='-')
        {   sign=-1;
             start=1;
         }


     for(i=start;i<len;i++)
     {
               if(st[i]!=' ')
                 x=(x+(st[i]-'0'))*10;
       } 

      x=x/10;

     x=x*sign;
     return(x);

}

        

