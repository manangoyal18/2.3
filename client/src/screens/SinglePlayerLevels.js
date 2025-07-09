import React from 'react';
import { View, Text, StyleSheet } from 'react-native';
import Button from '../components/Button';

export default function SinglePlayerLevels({ navigation }) {
  return (
    <View style={styles.container}>
      <Text style={styles.title}>Select Difficulty</Text>
      <View style={styles.buttonContainer}>
        <Button title="EASY" onPress={() => navigation.navigate('Easy')} />
        <Button title="MEDIUM" onPress={() => navigation.navigate('Medium')} />
        <Button title="BACK" onPress={() => navigation.goBack()} />
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#242D34',
    alignItems: 'center',
    justifyContent: 'center',
  },
  title: {
    fontSize: 36,
    fontWeight: 'bold',
    color: '#FFFFFF',
    marginBottom: 40,
  },
  buttonContainer: {
    width: '80%',
  },
});
