import React from 'react';
import { View, Text, StyleSheet } from 'react-native';
import { BackHandler } from 'react-native';
import Button from '../components/Button';

export default function HomeScreen({ navigation }) {
  return (
    <View style={styles.container}>
      <Text style={styles.title}>Tic Tac Toe</Text>
      
      <View style={styles.buttonContainer}>
        <Button 
          title="ONE PLAYER" 
          onPress={() => navigation.navigate('SinglePlayerLevels')}  // ✅ must match navigator key
        />
        <Button 
          title="TWO PLAYERS" 
          onPress={() => navigation.navigate('TwoPlayersType')} // ✅ must match navigator key
        />
        <Button 
          title="EXIT" 
           onPress={() => { 
           console.log('Exit pressed');
           BackHandler.exitApp();}} 
          style={styles.exitButton}
/>
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
    fontSize: 48,
    fontWeight: 'bold',
    color: '#FFFFFF',
    marginBottom: 50,
  },
  buttonContainer: {
    width: '80%',
  },
  exitButton: {
    backgroundColor: '#E74C3C',
  },
});
