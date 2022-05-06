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
  List(int, 3) new_li;
  List(int, 2) sub_li;
  int i;
  int dummy;
  
  new_li = [1, 2, 3];
  sub_li = new_li[1:3];

  dummy = sub_li[1];
  if (dummy == 3) {
      prints("correct");
  } else {

  }

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
