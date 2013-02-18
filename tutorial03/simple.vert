#version 140

// Input vertex data, different for all executions of this shader.
in vec3 vertexPosition_modelspace;
uniform mat4 MVP;

void main(){

    vec4 v = vec4(vertexPosition_modelspace, 1);
    gl_Position = MVP * v;

}

