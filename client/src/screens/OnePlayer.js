import React, { useEffect, useState } from 'react';
import { View, Text, Alert, StyleSheet, ImageBackground } from 'react-native';
import bg from '../../assets/bg.jpeg';
import Cell from '../components/Cell';

const emptyMap = [["", "", ""], ["", "", ""], ["", "", ""]];

const copyArray = (original) => original.map(arr => arr.slice());

export default function OnePlayer() {
  const [map, setMap] = useState(emptyMap);
  const [currentTurn, setCurrentTurn] = useState("x");

  useEffect(() => {
    if (currentTurn === "o") {
      botTurn();
    }
  }, [currentTurn]);

  useEffect(() => {
    const winner = getWinner(map);
    if (winner) gameWon(winner);
    else checkTieState();
  }, [map]);

  const onPress = (rowIndex, columnIndex) => {
    if (map[rowIndex][columnIndex] !== "") return;
    const updatedMap = [...map];
    updatedMap[rowIndex][columnIndex] = currentTurn;
    setMap(updatedMap);
    setCurrentTurn(currentTurn === "x" ? "o" : "x");
  };

  const getWinner = (map) => {
    for (let i = 0; i < 3; i++) {
      if (map[i].every(cell => cell === "x")) return "x";
      if (map[i].every(cell => cell === "o")) return "o";
    }

    for (let col = 0; col < 3; col++) {
      if (map.every(row => row[col] === "x")) return "x";
      if (map.every(row => row[col] === "o")) return "o";
    }

    if (map[0][0] === map[1][1] && map[1][1] === map[2][2] && map[0][0] !== "")
      return map[0][0];
    if (map[0][2] === map[1][1] && map[1][1] === map[2][0] && map[0][2] !== "")
      return map[0][2];
  };

  const checkTieState = () => {
    if (!map.some(row => row.includes(""))) {
      Alert.alert("It's a tie", "", [{ text: "Restart", onPress: resetGame }]);
    }
  };

  const gameWon = (player) => {
    Alert.alert("Hurray!", `Player ${player} won`, [{ text: "Restart", onPress: resetGame }]);
  };

  const resetGame = () => {
    setMap([["", "", ""], ["", "", ""], ["", "", ""]]);
    setCurrentTurn("x");
  };

  const botTurn = () => {
    const possiblePositions = [];
    map.forEach((row, rIdx) =>
      row.forEach((cell, cIdx) => {
        if (cell === "") possiblePositions.push({ row: rIdx, col: cIdx });
      })
    );

    let chosen = null;

    possiblePositions.forEach(pos => {
      const testMap = copyArray(map);
      testMap[pos.row][pos.col] = "o";
      if (getWinner(testMap) === "o") chosen = pos;
    });

    if (!chosen) {
      possiblePositions.forEach(pos => {
        const testMap = copyArray(map);
        testMap[pos.row][pos.col] = "x";
        if (getWinner(testMap) === "x") chosen = pos;
      });
    }

    if (!chosen) {
      chosen = possiblePositions[Math.floor(Math.random() * possiblePositions.length)];
    }

    if (chosen) onPress(chosen.row, chosen.col);
  };

  return (
    <View style={styles.container}>
      <ImageBackground source={bg} style={styles.bg} resizeMode="contain">
        <Text style={styles.turnText}>Current Turn: {currentTurn.toUpperCase()}</Text>
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
  container: { flex: 1, backgroundColor: "#242D34" },
  bg: { flex: 1, alignItems: "center", justifyContent: "center", paddingTop: 15 },
  turnText: { fontSize: 24, color: "white", position: "absolute", top: 50 },
  map: { width: "80%", aspectRatio: 1 },
  row: { flex: 1, flexDirection: "row" },
});
