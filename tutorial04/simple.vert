#version 140

// Input vertex data, different for all executions of this shader.
in vec3 vertexPosition_modelspace;
in vec3 vertexColor;
uniform mat4 MVP;

out vec3 fragmentColor;

void main(){

    fragmentColor = vertexColor;
    vec4 v = vec4(vertexPosition_modelspace, 1);
    gl_Position = MVP * v;

}

