public class HelloWorld{

     public static void main(String []args){
        System.out.println("Hello World");
        int i = 0;
        restart:
        
        System.out.println("new call");
        for(;;i++){
        	System.out.println(i);
        	if(i%5 == 0) 
        		continue restart;

        }
        
     }
}