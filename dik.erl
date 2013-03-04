-module(dik).
-export([read_graph/1]).
-export([dijkstra/2]).
-export([start/0]).

% The file contains an adjacency list representation of an undirected weighted graph.
% Each row consists of the node tuples that are adjacent to that particular vertex along with the length of that edge.
% Returns a dictionary of VertexNumber -> [Edge]
% 	Where Edge is {edge, {vertex, From}, {vertex, To}, Weight}
read_graph(FileName) ->
	{ok, Device} = file:open(FileName, [read]),
	Nodes = dict:new(),
	parse_rows(Device, Nodes).

parse_rows(Device, Nodes) ->
	case io:get_line(Device, "") of
		eof -> Nodes;
		Line -> [VertexNumString | Row] = string:tokens(Line, " \t\n"),
				VertexNum = list_to_integer(VertexNumString),
				case dict:is_key(VertexNum, Nodes) of
					false -> NewNodes = dict:store(VertexNum, [], Nodes)
				end,
				NewNodes2 = get_edges_from_line(NewNodes, VertexNum, Row),
				parse_rows(Device, NewNodes2)
	end.

parse_edge(Nodes, FromVertexNum, Val) ->
	[VertexString, WeightString] = string:tokens(Val, ","),
	[ToVertexNum, Weight] = [list_to_integer(VertexString), list_to_integer(WeightString)],
	dict:append(FromVertexNum, {edge, {vertex, FromVertexNum}, {vertex, ToVertexNum}, Weight}, Nodes).

get_edges_from_line(Nodes, _, []) -> 
	Nodes;

get_edges_from_line(Nodes, VertexNum, [Head | Tail]) -> 
	NewNodes = parse_edge(Nodes, VertexNum, Head),
	get_edges_from_line(NewNodes, VertexNum, Tail).


% Performs dijkstras shortest path algorithm on the given graph (Dictionary of VertexNumber -> [Edge]), starting at StartNode.
% Returns a Dictionary of VertexNumber -> Distance.
dijkstra(Nodes, StartNode) ->
	NewNodes = dict:map(fun(Key, Val) -> {if Key == StartNode -> 0; true -> infinity end, not_visited, Val} end, Nodes),
	dict:map(fun(_, {Distance, _, _}) -> Distance end, explore_nodes(NewNodes, StartNode, [StartNode])).

% Explore the given Nodes starting from NodeNum.
explore_nodes(Nodes, NodeNum, [NextNode|Unvisited]) ->
	{NewNodes, NewUnvisited} = explore_node(Nodes, NodeNum, Unvisited),
	explore_nodes(NewNodes, NextNode, NewUnvisited);

% Base case, no more nodes to explore.
explore_nodes(Nodes, _, []) ->
	Nodes.

% A convenience function to extract the node value from the dictionary so we can pattern match on whether it's been visited.
explore_node(Nodes, NodeNum, Unvisited) ->
	explore_node(Nodes, NodeNum, dict:fetch(NodeNum, Nodes), Unvisited).

% Already been visited: Do Nothing.
explore_node(Nodes, _, {_, visited, _}, Unvisited) ->
	{Nodes, Unvisited};

% Go through all the edges of the current node and update the tentative distance of the connected nodes.
% Add any unvisited nodes this node is connected to to the unvisited list.
explore_node(Nodes, NodeNum, {FromDistance, not_visited, [Edge|Edges]}, Unvisited) ->
	{edge, {vertex, NodeNum}, {vertex, ToVertexNum}, Weight} = Edge,
	{ToDistance, ToVisited, ToEdges} = dict:fetch(ToVertexNum, Nodes),
	if (ToDistance == infinity) orelse (Weight + FromDistance < ToDistance) ->
		NewNodes = dict:store(ToVertexNum, {Weight + FromDistance, ToVisited, ToEdges}, Nodes);
	   true ->
	   	NewNodes = Nodes
	end,
	if ToVisited == not_visited ->
		NewUnvisited = [ToVertexNum | Unvisited];
	   true ->
	    NewUnvisited = Unvisited
	end,
	explore_node(NewNodes, NodeNum, {FromDistance, not_visited, Edges}, NewUnvisited);

% Out of edges, this node has now been visited.
explore_node(Nodes, NodeNum, {FromDistance, not_visited, []}, Unvisited) ->
	% We have looped through all the edges so we need another dictionary fetch to get them back.
	{FromDistance, not_visited, OriginalEdges} = dict:fetch(NodeNum, Nodes),
	% Mark as visited.
	{dict:store(NodeNum, {FromDistance, visited, OriginalEdges}, Nodes), Unvisited}.

% Get the distances of a few nodes from the example graph.
start() ->
	DistanceNodes = [7,37,59,82,99,115,133,165,188,197],
	lists:map(fun(NodeNum) -> dict:fetch(NodeNum, dijkstra(read_graph("dijkstraData.txt"), 1)) end, DistanceNodes).