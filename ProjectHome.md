It's a prototype writing by scala. the syntax is not very rigid, for example the sampler is defined like this: void tex ： fs0(mipmap,mirror).  Any one who is interested can modify it in your own way.


Example:

float44 view : vc0

float44 model : vc1

float44 proj : vc2


float4 position : va0

float4 vertexColor : va1

float4 texcoord : va2


float4 fragmentColor : v0

float4 fragTexCoord : v1

void tex1 : fs0(mipmap, mirror)


void vertexMain()

{

> fragmentColor = vertexColor

> op = mul(mul(mul(position, model), view), proj)

> fragTexCoord = texcoord

}


void fragmentMain()

{

> oc = fragmentColor + tex2d(fragTexCoord, tex1)

> kil(oc)

}


generated code :

vertex shader:

mov v0, va1

mul vt1,va0,vc1

mul vt0,vt1,vc0

mul op,vt0,vc2

mov v1, va2


fragment shader:

tex2d ft0,v1,fs0<mipmap,mirror>

add oc,v0,ft0

kil oc