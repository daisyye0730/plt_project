int a;
int b;
###
int gcd(int a, int b) {
  while (a != b) {
    if (b < a) {
      a = a - b; }
    elif (b == a) {
      a = a + b;
    }
    (elif (b < a) {
      a = a * b;
    })
    else {b = b - a;}
  }
  print(a);
  return a;
}
###
int main() {
  int x;
  string str1;
  string str2;
  string str3;
  int val;
  List(string, 3) new_li;
  str1 = "abc";
  str2 = "bcd";
  x=5;
  new_li = ["hi", "not happy", "third"];
  new_li[2] = "bye";
  str1 = new_li[0];
  prints(str1);
  str2 = new_li[1];
  prints(str2);
  str3 = new_li[2];
  prints(str3);

  ###
  str1 = "abcd";
  str2 = "abcd";
  prints(str1);
  if (strcmp(str1, str2) == 0) {
      print(1);
  } else {
      print(21);
  }
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
