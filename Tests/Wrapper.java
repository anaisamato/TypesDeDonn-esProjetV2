import java.util.Scanner;

public class Wrapper {

    public static void main(String[] args){

        int i;
        i = MyClass.even(Integer.parseInt(args[0]), Integer.parseInt(args[1]), Integer.parseInt(args[2])/*, Integer.parseInt(args[3]), Integer.parseInt(args[4])*/);
        				 // Number of arguments = Number of variables in the environment
        System.out.println("result: "+ i);


    }

}
