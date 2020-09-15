struct Output {
    float4 color : TEXCOORD0;
    float4 pos : SV_Position;
};

Output main(float2 pos2 : POSITION0, float4 color : COLOR0) {
    Output o;

    o.pos = float4(pos2, 0.5, 1.0);
    o.color = color;
    
    return o;
}
