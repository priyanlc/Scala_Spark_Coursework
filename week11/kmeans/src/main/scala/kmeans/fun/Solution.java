package kmeans.fun;

/**
 * Created by priyanchandrapala on 19/05/2017.
 */
public class Solution extends Thread{

    static Thread laurel, hardy;
    public static void main(String[] args) throws InterruptedException {
        laurel = new Thread() {
            public void run() {
                System.out.println("A");
                try {
                    hardy.sleep(5000);
                } catch (Exception e) {
                    System.out.println("B");
                }
                System.out.println("C");
            }
        };
        hardy = new Thread() {
            public void run() {
                System.out.println("D");
                try {
                    synchronized(laurel){
                        laurel.wait();
                        //wait method is called here,
                        //There is not notify in this class,
                        //but the statement below are executing
                        System.out.println(Thread.currentThread().getName());
                    }
                } catch (Exception e) {
                    System.out.println("E");
                }
                System.out.println("F");
            }
        };
        laurel.setName("laurel");
        hardy.setName("hardy");
        laurel.start();
        hardy.start();
    }
}
