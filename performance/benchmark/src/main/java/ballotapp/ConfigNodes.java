package ballotapp;

import java.io.File;
import java.io.PrintWriter;
import java.util.Scanner;

public class ConfigNodes {
  private static final String key =
      "30819D300D06092A864886F70D010101050003818B0030818702818100C710BF20C0B603A4C4028FFEBD4E0647FF4DFB8D23A3E3F39E50A8B2B05A3335A92641F7227C0EF803E51586F1DEE07EE3D789A7A55484239ED359E8C063D694DDCF9DCD38F59AF228D9EF1ADDE63C9324D7E0495E53A029BC775D2670A285AAAEE4413C79AD7A61063A232AC8965CA35CF72A70BF56160DE35FA68E9D2B25D1020111";
  private static String input = "nodes/s16_reps/s_f5c0_config";
  private static String output = "nodes/config-public/s_f5c0_config.pub";
  private static String ips = "nodes/internal_ip.txt";
  private static int N = 16; // number of replicas
  private static int M = 16; // number of clients of each replica

  public static void main(String[] args) throws Exception {

    int num = Integer.valueOf(args[0]);
    if (num == 4) {
      N = 4;
      M = 4;
      input = "nodes/s4_reps/s_f1c0_config";
      output = "nodes/config-public/s_f1c0_config.pub";
    }

    Scanner scanner = new Scanner(new File(input));
    PrintWriter out = new PrintWriter(output);
    while (scanner.hasNextLine()) {
      String line = scanner.nextLine();
      if (line.startsWith("rep") || line.startsWith("client") || line.startsWith("3081")) {
        int skip = M * (N + 1) * 2;
        for (int i = 1; i < skip; i++) {
          line = scanner.nextLine();
        }
        Scanner ipscanner = new Scanner(new File(ips));
        for (int i = 0; i < N; i++) {
          String host = ipscanner.nextLine();
          out.printf("%s %s 3501\n", host, host);
          out.printf(key + "\n");
        }

        ipscanner = new Scanner(new File(ips));
        for (int i = 0; i < N; i++) {
          String host = ipscanner.nextLine();
          for (int j = 0; j < M; j++) {
            out.printf("%s %s %d\n", host, host, 3502 + j);
            out.printf(key + "\n");
          }
        }
        ipscanner.close();
      } else {
        out.write(line + "\n");
      }
    }
    out.close();
    scanner.close();
  }
}
