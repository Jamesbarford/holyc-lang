#define M_E         2.71828182845904523536028747135266250   /* e              */
#define M_LOG2E     1.44269504088896340735992468100189214   /* log2(e)        */
#define M_LOG10E    0.434294481903251827651128918916605082  /* log10(e)       */
#define M_LN2       0.693147180559945309417232121458176568  /* loge(2)        */
#define M_LN10      2.30258509299404568401799145468436421   /* loge(10)       */
#define M_PI        3.14159265358979323846264338327950288   /* pi             */
#define M_PI_2      1.57079632679489661923132169163975144   /* pi/2           */
#define M_PI_4      0.785398163397448309615660845819875721  /* pi/4           */
#define M_1_PI      0.318309886183790671537767526745028724  /* 1/pi           */
#define M_2_PI      0.636619772367581343075535053490057448  /* 2/pi           */
#define M_2_SQRTPI  1.12837916709551257389615890312154517   /* 2/sqrt(pi)     */
#define M_SQRT2     1.41421356237309504880168872420969808   /* sqrt(2)        */
#define M_SQRT1_2   0.707106781186547524400844362104849039  /* 1/sqrt(2)      */


/* Libc's maths functions */
extern "c" F64 acos(F64 f1);
extern "c" F64 asin(F64 f1);
extern "c" F64 atan(F64 f1);
extern "c" F64 atan2(F64 f1, F64 f2);
extern "c" F64 cos(F64 f1);
extern "c" F64 sin(F64 f1);
extern "c" F64 tan(F64 f1);
extern "c" F64 acosh(F64 f1);
extern "c" F64 asinh(F64 f1);
extern "c" F64 atanh(F64 f1);
extern "c" F64 cosh(F64 f1);
extern "c" F64 sinh(F64 f1);
extern "c" F64 tanh(F64 f1);
extern "c" F64 exp(F64 f1);
extern "c" F64 exp2(F64 f1); 
extern "c" F64 expm1(F64 f1); 
extern "c" F64 log(F64 f1);
extern "c" F64 log10(F64 f1);
extern "c" F64 log2(F64 f1);
extern "c" F64 log1p(F64 f1);
extern "c" F64 logb(F64 f1);
extern "c" F64 modf(F64 f1, F64 *fptr);
extern "c" F64 ldexp(F64 f1, I32 i);
extern "c" F64 frexp(F64 f1, I32 *i);
extern "c" I32 ilogb(F64 f1);
extern "c" F64 scalbn(F64 f1, I32 i);
extern "c" F64 scalbln(F64 f1, I64 i);
extern "c" F64 fabs(F64 f1);
extern "c" F64 cbrt(F64 f1);
extern "c" F64 hypot(F64 f1, F64 f2);
extern "c" F64 pow(F64 f1, F64 f2);
extern "c" F64 sqrt(F64 f1);
extern "c" F64 erf(F64 f1);
extern "c" F64 erfc(F64 f1);
extern "c" F64 lgamma(F64 f1);
extern "c" F64 tgamma(F64 f1);
extern "c" F64 ceil(F64 f1);
extern "c" F64 floor(F64 f1);
extern "c" F64 nearbyint(F64 f1);
extern "c" F64 rint(F64 f1);
extern "c" I64 lrint(F64 f1);
extern "c" F64 round(F64 f1);
extern "c" I64 lround(F64 f1);
extern "c" F64 trunc(F64 f1);
extern "c" F64 fmod(F64 f1, F64 f2);
extern "c" F64 remainder(F64 f1, F64 f2);
extern "c" F64 remquo(F64 f1, F64 f2, I32 *ptr);
extern "c" F64 copysign(F64 f1, F64 f2);
extern "c" F64 nan(U8 *s);
extern "c" F64 nextafter(F64 f1, F64 f2);
extern "c" F64 fdim(F64 f1, F64 f2);
extern "c" F64 fmax(F64 f1, F64 f2);
extern "c" F64 fmin(F64 f1, F64 f2);
extern "c" F64 fma(F64 f1, F64 f2, F64 f3);

asm {
_ABS::
    MOV     RAX, RDI
    NEG     RAX
    CMOVL   RAX, RDI /* will discard the negate if the value was greater than 0 */
    RET

/* Non recursive greatest common divisor function */
_GCD::
    TEST    RSI, RSI
    MOV     RAX, RDI
    JNE     @@02
    JMP     @@01
@@03:
    MOV     RDX, RSI
@@02:
    CQTO
    IDIV    RSI
    TEST    RDX, RDX
    MOV     RAX, RSI
    JNE     @@03
    MOVQ    RAX, RSI
    RET
@@01:
    MOV RAX, RDI
    RET
}

public _extern _ABS U64 Abs(I64 num);
public _extern _GCD U64 Gcd(I64 a, I64 b);

F64 Pow(F64 a, I64 b)
{
  if (b == 0) return 1.0;
  F64 result = 1.0;
  if (b < 0) {
    a = 1 / a;
    b = -b;
  }
  while (b > 0) {
    if (b & 2 != 1) {
      result = result * a;
    }
    a = a * a;
    b /= 2;
  }
  return result;
}

U64 ModU64(U64 *k, U64 m)
{
  return *k % m;
}

