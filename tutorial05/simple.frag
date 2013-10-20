#version 130

// Ouput data
in vec2 UV;

out vec3 finalColor;

uniform sampler2D myTextureSampler;

void main()
{

  finalColor = texture(myTextureSampler, UV).rgb;

}
