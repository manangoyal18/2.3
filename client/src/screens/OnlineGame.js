import React, { useEffect, useState, useRef } from 'react';
import { View, Text, Alert, StyleSheet, ImageBackground } from 'react-native';
import bg from '../../assets/bg.jpeg';
import Cell from '../components/Cell';

const emptyBoard = [["", "", ""], ["", "", ""], ["", "", ""]];

export default function OnlineGame() {
  const [map, setMap] = useState([["", "", ""], ["", "", ""], ["", "", ""]]);
  const [currentTurn, setCurrentTurn] = useState("x");
  const [playerSymbol, setPlayerSymbol] = useState(null); // "x" or "o"
  const [connected, setConnected] = useState(false);
  const socketRef = useRef(null);

  useEffect(() => {
    // Connect to Erlang WebSocket server
    socketRef.current = new WebSocket('ws://172.0.10.38:8080/websocket');

    socketRef.current.onopen = () => {
      console.log('Connected to server');
      setConnected(true);
    };

    socketRef.current.onmessage = (message) => {
      const data = JSON.parse(message.data);
      console.log("Received from server:", JSON.stringify(data, null, 2));

      switch (data.type) {
        case 'assign_symbol':
          setPlayerSymbol(data.symbol);
          break;
        case 'game_state':
          setMap(data.board);
          setCurrentTurn(data.turn);
          break;
        case 'game_result':
          Alert.alert("Game Over", data.result, [{ text: "OK", onPress: resetGame }]);
          break;
        default:
          console.warn("Unknown message type:", data.type);
      }
    };

    socketRef.current.onerror = (err) => {
      console.error("Socket error:", err.message);
    };

    socketRef.current.onclose = () => {
      console.log("Connection closed");
      Alert.alert("Disconnected", "Lost connection to the game server.");
    };

    return () => {
      socketRef.current?.close();
    };
  }, []);

  const resetGame = () => {
    setMap([["", "", ""], ["", "", ""], ["", "", ""]]);
    setCurrentTurn("x");
    socketRef.current?.send(JSON.stringify({ type: 'reset' }));
  };

  const onPress = (rowIndex, columnIndex) => {
    if (!playerSymbol) return;
    if (map[rowIndex][columnIndex] !== "") return;
    if (currentTurn !== playerSymbol) return;

    const payload = {
      type: "move",
      row: rowIndex,
      col: columnIndex,
      symbol: playerSymbol,
    };

    console.log("Sending move:", payload);
    socketRef.current.send(JSON.stringify(payload));
  };

  if (!connected || !playerSymbol) {
    return (
      <View style={styles.loadingContainer}>
        <Text style={styles.loadingText}>Connecting to server...</Text>
      </View>
    );
  }

  return (
    <View style={styles.container}>
      <ImageBackground source={bg} style={styles.bg} resizeMode="contain">
        <Text style={styles.turnText}>You are: {playerSymbol?.toUpperCase()}</Text>
        <Text style={styles.turnText2}>Current Turn: {currentTurn?.toUpperCase()}</Text>
        <View style={styles.map}>
          {map.map((row, rIdx) => (
            <View key={rIdx} style={styles.row}>
              {row.map((cell, cIdx) => (
                <Cell key={cIdx} cell={cell} onPress={() => onPress(rIdx, cIdx)} />
              ))}
            </View>
          ))}
        </View>
      </ImageBackground>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: "#242D34",
  },
  loadingContainer: {
    flex: 1,
    justifyContent: "center",
    alignItems: "center",
    backgroundColor: "#242D34",
  },
  loadingText: {
    fontSize: 22,
    color: "white",
  },
  bg: {
    flex: 1,
    alignItems: "center",
    justifyContent: "center",
    paddingTop: 35,
  },
  turnText: {
    fontSize: 24,
    color: "white",
    position: "absolute",
    top: 150,
  },
  turnText2: {
    fontSize: 24,
    color: "white",
    position: "absolute",
    top: 50,
  },
  map: {
    width: "80%",
    aspectRatio: 1,
  },
  row: {
    flex: 1,
    flexDirection: "row",
  },
});
