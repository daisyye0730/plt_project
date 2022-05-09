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
  int a;
  int b;
  int dummy;
  
  for(i=0; i<3; i++){
    new_li[i] = i;
  }

  old_li = [5, 4, 4];
  r_li = old_li - new_li;

  for(i=0; i<3; i++){
    print(r_li[i]);
  }

  return 0;
}
