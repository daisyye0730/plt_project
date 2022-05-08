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
  List(int, 3) old_li;
  List(int, 3) r_li;
  List(int, 2) sub_li;
  int i;
  int dummy;
  
  new_li = [1, 2, 3];
  old_li = [5, 4, 4];
  sub_li = new_li[1:3];
  r_li = old_li - new_li;

  print(r_li[0]);

  print(r_li[1]);

  print(r_li[2]);

  return 0;
}
