package kmeans.fun;

import java.text.SimpleDateFormat;
import java.util.Random;

/**
 * Created by priyanchandrapala on 16/05/2017.
 */
public class ThreadLocalExample implements Runnable {


    private static final ThreadLocal<String> name = new ThreadLocal<String>(){
        @Override
        protected String initialValue()
        {
            return new String("Priyan");
        }
    };

    public static void main(String[] args) throws InterruptedException {

        ThreadLocalExample obj = new ThreadLocalExample();

        for(int i=0 ; i<10; i++){
            Thread t = new Thread(obj, ""+i);
            Thread.sleep(new Random().nextInt(1000));
            t.start();
        }

    }

    @Override
    public void run() {
        System.out.println("Thread Name= "+Thread.currentThread().getName()+" default Formatter = "+name.get());
        try {
            Thread.sleep(new Random().nextInt(1000));
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        name.set("Lakmal");

        System.out.println("Thread Name= "+Thread.currentThread().getName()+" formatter = "+name.get());
    }

}
