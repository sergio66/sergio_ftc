%{
http://karpathy.github.io/neuralnets/
https://theclevermachine.wordpress.com/2014/09/11/a-gentle-introduction-to-artificial-neural-networks/
http://www2.econ.iastate.edu/tesfatsi/NeuralNetworks.CheungCannonNotes.pdf
%}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% https://www.mathworks.com/help/nnet/gs/fit-data-with-a-neural-network.html

%% load data
load house_dataset
inputs = houseInputs;
targets = houseTargets;

%% create a network with 10 neurons
hiddenLayerSize = 10;
net = fitnet(hiddenLayerSize);

%% how to divide the data
net.divideParam.trainRatio = 70/100;
net.divideParam.valRatio = 15/100;
net.divideParam.testRatio = 15/100;

%% train
net.trainFcn = 'trainbr';
net.trainFcn = 'trainscg';
[net,tr] = train(net,inputs,targets);

%% test network
outputs = net(inputs);
errors = gsubtract(targets,outputs);
performance = perform(net,targets,outputs)

disp('click regression')


%% view network
view(net)