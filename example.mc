int main() {
    List(int, 3) li1;
    List(int, 3) li2;
    List(int, 4) sum;
    int i;

    li1 = [2, 3, 4];
    li2 = [-2, -3, -4];

    sum = li1+li2;

    for (i = 0; i < 3; i++) {
      print(sum[i]);
    }
}