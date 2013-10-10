#version 140

// Ouput data
out vec3 color;

varying vec3 fragmentColor;

void main()
{

	// Output color = red 
	//color = vec3(1,0,0);
  color = fragmentColor;

}
