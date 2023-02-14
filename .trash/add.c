// add.cpp
struct Vec2 {
    float x;
    float y;
};
__attribute__((used)) Vec2 add (Vec2 a, Vec2 b)
{
    Vec2 ret{0.0, 0.0};
    ret.x = a.x + b.x;
    ret.y = a.y + b.y;
    return ret;
}