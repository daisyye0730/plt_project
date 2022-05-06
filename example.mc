int a;
int b;

int main() {
  int x;
  int y;
  int z;
  string str1;
  string str2;
  bool val;
 
  x = 2;
  y = 5;
  str1 = "abcd";
  str2 = "abcd";
  if (strcmp(str1, str2) == 0) {
      print(1);
  } else {
      print(21);
  }
    ###
  if (x > y) {
      z = x - y;
      print(z);
  } elif (x < y) {
      z = y - x;
      print(z);
  } (elif ((x-1) > y) {
      z = y - x;
      print(z);
  }) (elif ((x+1)< y) {
      z = 22;
      print(z);
  }) else {
      z = 1;
      print(z);
  }

  while (x < y) {
      x ++;
      if (x == y - 1) {
          print(z);
          break;
      } else {

      }
      print(x);
  }

  for (x = 1; x < 5; x++) {
      print(x);
      if (x == 2) {
        break;
      } else {

      }
  }
  ###

  return 0;
}
