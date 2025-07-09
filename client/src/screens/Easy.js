import React, { useState, useEffect } from 'react';
import { View, Text, StyleSheet, Alert, ImageBackground } from 'react-native';
import Cell from '../components/Cell';
import bg from '../../assets/bg.jpeg';

const emptyMap = [
  ["", "", ""],
  ["", "", ""],
  ["", "", ""],
];

const copyArray = (original) => original.map((arr) => arr.slice());

export default function Easy() {
  const [map, setMap] = useState(emptyMap);
  const [currentTurn, setCurrentTurn] = useState("x");

  useEffect(() => {
    if (currentTurn === "o") {
      botTurn();
    }
  }, [currentTurn]);

  useEffect(() => {
    const winner = getWinner(map);
    if (winner) {
      gameWon(winner);
    } else {
      checkTieState();
    }
  }, [map]);

  const onPress = (rowIndex, columnIndex) => {
    if (map[rowIndex][columnIndex] !== "") return;

    setMap((existingMap) => {
      const updatedMap = [...existingMap];
      updatedMap[rowIndex][columnIndex] = currentTurn;
      return updatedMap;
    });

    setCurrentTurn(currentTurn === "x" ? "o" : "x");
  };

  const getWinner = (winnerMap) => {
    for (let i = 0; i < 3; i++) {
      if (winnerMap[i].every(cell => cell === 'x')) return 'x';
      if (winnerMap[i].every(cell => cell === 'o')) return 'o';
    }
    for (let col = 0; col < 3; col++) {
      if (winnerMap.every(row => row[col] === 'x')) return 'x';
      if (winnerMap.every(row => row[col] === 'o')) return 'o';
    }
    if ([0, 1, 2].every(i => winnerMap[i][i] === 'x')) return 'x';
    if ([0, 1, 2].every(i => winnerMap[i][i] === 'o')) return 'o';
    if ([0, 1, 2].every(i => winnerMap[i][2 - i] === 'x')) return 'x';
    if ([0, 1, 2].every(i => winnerMap[i][2 - i] === 'o')) return 'o';
  };

  const checkTieState = () => {
    if (!map.some(row => row.includes(""))) {
      Alert.alert("It's a tie", "", [{ text: "Restart", onPress: resetGame }]);
    }
  };

  const gameWon = (player) => {
    Alert.alert(`Player ${player} won`, "", [{ text: "Restart", onPress: resetGame }]);
  };

  const resetGame = () => {
    setMap([["", "", ""], ["", "", ""], ["", "", ""]]);
    setCurrentTurn("x");
  };

  const botTurn = () => {
    const possible = [];
    map.forEach((row, rowIndex) =>
      row.forEach((cell, colIndex) => {
        if (cell === "") possible.push({ row: rowIndex, col: colIndex });
      })
    );

    const chosen = possible[Math.floor(Math.random() * possible.length)];
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
  bg: { flex: 1, alignItems: "center", justifyContent: "center", paddingTop: 35 },
  turnText: { fontSize: 24, color: "white", position: "absolute", top: 50 },
  map: { width: "80%", aspectRatio: 1 },
  row: { flex: 1, flexDirection: "row" },
});
